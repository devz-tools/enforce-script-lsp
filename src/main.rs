mod lexer;
mod lsp;
mod parser;
mod semantic;
mod utils;
mod workspace;

use tokio::main as tokio_main;

#[tokio_main]
async fn main() {
    lsp::start_server().await;
}
