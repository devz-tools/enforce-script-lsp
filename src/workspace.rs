use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use regex::Regex;

/// One module's `config.cpp` (a `CfgMods` entry).
#[derive(Debug, Clone)]
pub struct ModuleConfig {
    /// Absolute path of the `config.cpp` file.
    pub path: PathBuf,
    /// The `dir` field (project-relative), if present.
    #[allow(dead_code)] // used for project-root discovery (next step)
    pub dir: Option<String>,
    /// All `files[]` entries (e.g. `"dm_cons/3_Game"` or a full project path).
    pub files: Vec<String>,
    /// All `defines[]` entries (active preprocessor defines for this module).
    #[allow(dead_code)] // used for defines-aware #ifdef (next step)
    pub defines: Vec<String>,
}

impl ModuleConfig {
    /// Directory that holds this `config.cpp` (the module's script root).
    pub fn directory(&self) -> Option<&Path> {
        self.path.parent()
    }
}

/// Parse a `config.cpp` source, extracting `dir`, `files[]` and `defines[]`.
pub fn parse_config(path: &Path, text: &str) -> ModuleConfig {
    ModuleConfig {
        path: path.to_path_buf(),
        dir: extract_string(text, "dir"),
        files: extract_string_array(text, "files"),
        defines: extract_string_array(text, "defines"),
    }
}

/// Extract a single `key = "value"` string assignment.
fn extract_string(text: &str, key: &str) -> Option<String> {
    let re = Regex::new(&format!(r#"{}\s*=\s*"([^"]*)""#, key)).ok()?;
    re.captures(text).map(|c| c[1].to_string())
}

/// Extract all string literals from every `key[] = { "a", "b" }` array in the text.
/// `files[]` appears once per script module (game/world/mission), so we gather all.
fn extract_string_array(text: &str, key: &str) -> Vec<String> {
    let mut result = Vec::new();
    let array_re = match Regex::new(&format!(r#"{}\[\]\s*=\s*\{{([^}}]*)\}}"#, key)) {
        Ok(re) => re,
        Err(_) => return result,
    };
    let str_re = match Regex::new(r#""([^"]*)""#) {
        Ok(re) => re,
        Err(_) => return result,
    };

    for caps in array_re.captures_iter(text) {
        let inner = &caps[1];
        for s in str_re.captures_iter(inner) {
            result.push(s[1].to_string());
        }
    }
    result
}

/// Split a `files[]` entry into (prefix, last-segment).
/// `"dm_cons/3_Game"` → `("dm_cons", "3_Game")`.
/// `"DayZExpansion/Core/Scripts/3_Game"` → `("DayZExpansion/Core/Scripts", "3_Game")`.
fn split_prefix(entry: &str) -> (&str, &str) {
    match entry.rfind('/') {
        Some(idx) => (&entry[..idx], &entry[idx + 1..]),
        None => (entry, ""),
    }
}

/// Project-level model: all modules' configs, the prefix→directory map, and the
/// set of directories that actually enter compilation.
#[derive(Debug, Default)]
pub struct Workspace {
    #[allow(dead_code)] // used for dependency/vanilla roots (later steps)
    pub root: PathBuf,
    /// Extra source roots to index "лайтово" (vanilla, Expansion, ...): all `.c`
    /// files under them, regardless of config.cpp.
    pub dependency_roots: Vec<PathBuf>,
    #[allow(dead_code)] // used for per-module defines/index (later steps)
    pub configs: Vec<ModuleConfig>,
    /// `files[]` prefix → directory of the `config.cpp` that declared it.
    #[allow(dead_code)] // used for cross-module resolution (later steps)
    pub prefix_map: HashMap<String, PathBuf>,
    /// Absolute paths of every compiled directory (module layer).
    pub compiled_dirs: HashSet<PathBuf>,
    /// Default preprocessor defines applied to every file (e.g. engine feature
    /// flags like `FEATURE_NETWORK_RECONCILIATION`), configured via
    /// `defaultDefines` in initializationOptions. Merged into `defines_for`.
    pub default_defines: HashSet<String>,
}

impl Workspace {
    /// Discover every `config.cpp` under `root` and build the workspace model.
    /// `dependency_roots` are extra roots (vanilla/Expansion) indexed separately.
    pub fn build(
        root: &Path,
        dependency_roots: Vec<PathBuf>,
        default_defines: Vec<String>,
    ) -> Workspace {
        let mut configs = Vec::new();
        for path in discover_config_files(root) {
            if let Ok(text) = std::fs::read_to_string(&path) {
                configs.push(parse_config(&path, &text));
            }
        }

        // prefix → directory: learn from every config.cpp that declares it.
        let mut prefix_map: HashMap<String, PathBuf> = HashMap::new();
        for cfg in &configs {
            let Some(dir) = cfg.directory() else { continue };
            for entry in &cfg.files {
                let (prefix, _) = split_prefix(entry);
                if !prefix.is_empty() {
                    prefix_map.insert(prefix.to_string(), dir.to_path_buf());
                }
            }
        }

        // compiled dirs: map[prefix] + "/" + sub, for each files[] entry.
        let mut compiled_dirs = HashSet::new();
        for cfg in &configs {
            let dir = cfg.directory();
            for entry in &cfg.files {
                let (prefix, sub) = split_prefix(entry);
                let base = match (dir, prefix_map.get(prefix)) {
                    (_, Some(d)) => Some(d.clone()),
                    (Some(d), None) => Some(d.to_path_buf()),
                    (None, None) => None,
                };
                if let Some(base) = base {
                    if sub.is_empty() {
                        compiled_dirs.insert(base);
                    } else {
                        compiled_dirs.insert(base.join(sub));
                    }
                }
            }
        }

        Workspace {
            root: root.to_path_buf(),
            dependency_roots,
            configs,
            prefix_map,
            compiled_dirs,
            default_defines: default_defines.into_iter().collect(),
        }
    }

    /// Whether a script file lives under a compiled directory.
    pub fn is_compiled(&self, path: &Path) -> bool {
        self.compiled_dirs
            .iter()
            .any(|dir| path.starts_with(dir))
    }

    /// Active `defines[]` for the module that owns `path`, found by walking up
    /// from the file to the nearest `config.cpp`.
    pub fn defines_for(&self, path: &Path) -> HashSet<String> {
        let mut defines = self.default_defines.clone();
        let mut dir = path.parent();
        while let Some(d) = dir {
            let config = d.join("config.cpp");
            if let Some(cfg) = self.configs.iter().find(|c| c.path == config) {
                defines.extend(cfg.defines.iter().cloned());
                return defines;
            }
            dir = d.parent();
        }
        defines
    }
}

/// Recursively find all `config.cpp` files under `root`, skipping junk dirs.
fn discover_config_files(root: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    walk(root, &mut out);
    out
}

fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else { return };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
                continue;
            };
            if name.starts_with('.') || name == "target" || name == "node_modules" {
                continue;
            }
            walk(&path, out);
        } else if path.file_name().and_then(|n| n.to_str()) == Some("config.cpp") {
            out.push(path);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_prefix() {
        assert_eq!(split_prefix("dm_cons/3_Game"), ("dm_cons", "3_Game"));
        assert_eq!(
            split_prefix("DayZExpansion/Core/Scripts/3_Game"),
            ("DayZExpansion/Core/Scripts", "3_Game")
        );
        assert_eq!(split_prefix("noprefix"), ("noprefix", ""));
    }

    #[test]
    fn test_parse_config_prefix_style() {
        let src = r#"
class CfgMods
{
	class dmBotorama_Cons
	{
		defines[] = { "DM_BOT_PROFILE" };
		class defs
		{
			class gameScriptModule { files[] = { "dm_cons/3_Game" }; };
			class worldScriptModule { files[] = { "dm_cons/4_World" }; };
			class missionScriptModule { files[] = { "dm_cons/5_Mission" }; };
		};
	};
};
"#;
        let cfg = parse_config(Path::new("/proj/src/cons/config.cpp"), src);
        assert_eq!(cfg.dir, None);
        assert_eq!(cfg.defines, vec!["DM_BOT_PROFILE"]);
        assert_eq!(
            cfg.files,
            vec!["dm_cons/3_Game", "dm_cons/4_World", "dm_cons/5_Mission"]
        );
    }

    #[test]
    fn test_parse_config_full_path_style() {
        let src = r#"
class CfgMods
{
	class DZ_Expansion_Core
	{
		dir="DayZExpansion/Core";
		class defs
		{
			class gameScriptModule { files[] = { "DayZExpansion/Core/Scripts/3_Game" }; };
			class worldScriptModule { files[] = { "DayZExpansion/Core/Scripts/4_World" }; };
		};
	};
};
"#;
        let cfg = parse_config(Path::new("/proj/DayZExpansion/Core/Scripts/config.cpp"), src);
        assert_eq!(cfg.dir.as_deref(), Some("DayZExpansion/Core"));
        assert_eq!(
            cfg.files,
            vec![
                "DayZExpansion/Core/Scripts/3_Game",
                "DayZExpansion/Core/Scripts/4_World"
            ]
        );
    }

    #[test]
    fn test_workspace_build_and_is_compiled() {
        let root = std::env::temp_dir().join("enf_lsp_ws_test");
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(root.join("src/cons/3_Game")).unwrap();
        std::fs::create_dir_all(root.join("src/cons/4_World")).unwrap();
        std::fs::write(
            root.join("src/cons/config.cpp"),
            r#"
class CfgMods
{
	class M { class defs {
		class gameScriptModule { files[] = { "dm_cons/3_Game" }; };
		class worldScriptModule { files[] = { "dm_cons/4_World" }; };
	}; };
};
"#,
        )
        .unwrap();

        let ws = Workspace::build(&root, Vec::new(), Vec::new());
        assert_eq!(ws.configs.len(), 1);
        assert!(ws.is_compiled(&root.join("src/cons/3_Game/foo.c")));
        assert!(ws.is_compiled(&root.join("src/cons/4_World/bar.c")));
        assert!(!ws.is_compiled(&root.join("src/cons/unlisted.c")));

        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn test_defines_for_applies_default_defines_to_vanilla_path() {
        let root = std::env::temp_dir().join("enf_lsp_ws_defines_test");
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(&root).unwrap();

        let ws = Workspace::build(&root, Vec::new(), vec!["FEATURE_X".to_string()]);

        // A path with no config.cpp anywhere up the tree must still see the
        // default defines (vanilla/engine files have no config.cpp).
        let defines = ws.defines_for(Path::new("/nonexistent/vanilla/foo.c"));
        assert!(defines.contains("FEATURE_X"));

        let _ = std::fs::remove_dir_all(&root);
    }
}
