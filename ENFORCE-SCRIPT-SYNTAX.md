# Enforce Script Syntax

This information was directly transcribed to markdown from Bohemia Interactive's website on the [Enforce Script Syntax](https://community.bistudio.com/wiki/DayZ:Enforce_Script_Syntax).
The content of this document is simply included in this format for convenience.

**DayZ**

Enforce Script is the language that is used by the Enfusion engine first introduced in DayZ Standalone. It is a Object-Oriented Scripting Language (OOP) that works with objects and classes and is similar to C# programming language.

> **Arma Reforger**  
> For Arma Reforger, see Arma Reforger Scripting Tutorials starting with Arma Reforger:From SQF to Enforce Script.

## Basics

### Code blocks

Code block is bordered by curly brackets and defines a scope. Variables defined inside scope are accessible only from inside of it.

**Scope example**

```cpp
void Hello()
{
	int x = 2; // First declaration of x
}

void Hello2()
{
	int x = 23; // Different x not related to the first one
}
```

**Nested scope**

```cpp
void World()
{
	int i = 5;

	// Following i is in a nested scope (scope of the for loop)
	// Therefore we get an error because multiple declaration is not allowed
	for (int i = 0; i < 12; ++i)
	{
	}
}
```

**Scope statements**

```cpp
void Hello()
{
	if (true)
	{
		// This is code block for the if branch
		int x = 2; // First declaration of x
	}
	else
	{
		// This is code block for the else branch
		int x = 23; // This will currently throw a multiple declaration error - while this should change for future enforce script iterations, it might stay like this in DayZ. To circumvent this, define the x above the if statement or use different variables.
	}
}
```

### Program structure

Enforce Script consists of classes and functions. All code must be declared inside a function.

```cpp
class MyClass
{
	void Hello()
	{
		Print("Hello World"); // ok
	}
}

void Hello()
{
	Print("Hello World"); // ok
}

Print("Hello World"); // this code will be never executed, and should be caught by compiler as unexpected statement
```

### Variables

Variables are defined by type and name.

```cpp
void Test()
{
	// declare
	int a;

	// assign
	a = 5;

	// initialize
	int b = 9;
}
```

### Functions

Functions are basic feature of Enforce Script. Function declaration consist of return value type, function name and list of parameters.

- Functions can be declared in global scope or inside class declaration
- Function parameters are fixed and typed (cannot be changed during run-time, no variadic parameters)
- Functions can be overloaded
- Keyword 'out' before parameter declaration ensures, that the parameter is passed by reference (you can pass more than one value from a function this way, can be used only in native functions)
- Enforce Script supports default parameter

```cpp
void MethodA()  // function with no parameters and no return value
{
}

int GiveMeTen() // function with no parameters which returns integer value
{
	return 10;
}

void GiveMeElevenAndTwelve(out int val1, out int val2, int val3) // function with 2 of the parameters passed as reference
{
	val1 = 11;
	val2 = 12;
	val3 = 13;
}

void PrintNum(int a = 0) // example of function with default parameter
{
	Print(a);
}

void MethodB()
{
	int ten = 0;
	int eleven = 0;
	int twelve = 0;
	int thirteen = 0;

	ten = GiveMeTen();

	// function "GiveMeElevenAndTwelve" sets values of "eleven" and "twelve" variables,
	// because "val1" and "val2" parameters are marked with "out" keyword,
	// but value of "thirteen" variable is not changed, because third parameter is not marked as "out" and "val3"
	// behaves only like a local variable of "GiveMeElevenAndTwelve" function
	GiveMeElevenAndTwelve(eleven, twelve, thirteen);

	Print(ten); // prints "ten = 10"
	Print(eleven); // prints "eleven = 11"
	Print(twelve); // prints "twelve = 12"
	Print(thirteen ); // prints "thirteen = 0"

	PrintNum(); // function "PrintNum" has default parameter, so its ok to call with empty brackets, it prints "a = 0"
	PrintNum(7); // prints "a = 7"
}

float Sum(float a, float b) // function with two float parameters which return float value
{
	return a + b;
}

float Sum(int a, int b)	// overloaded Sum function which uses int parameters instead
{
	return a + b;
}


void PrintCount(TStringArray stringArray) // function with one "TStringArray" object parameter which returns no value
{
	if (!stringArray) // check if stringArray is not null
		return;

	int count = stringArray.Count();
	Print(count);
}
```

### Comments

```cpp
/*
Multi
line
comment
*/

void Test()
{
	Print("Hello"); // single line comment
}
```

### Constants

Constants are like variables but read only. They are declared by const keyword.

```cpp
const int MONTHS_COUNT = 12;

void Test()
{
	int a = MONTHS_COUNT; // ok
	MONTHS_COUNT = 7; // err! you cannot change constant!
}
```

## Operators

**Operator Priority:** Priority of operators is similar to C language, more info.

### Arithmetic Operators

| Operation | Symbol |
| --------- | ------ |
| Add       | +      |
| Subtract  | -      |
| Multiply  | *      |
| Divide    | /      |
| Modulo    | %      |

### Assignments

| Operation                     | Symbol |
| ----------------------------- | ------ |
| Assign value to variable      | =      |
| Increment variable by value   | +=     |
| Decrement variable by value   | -=     |
| Multiply variable by value    | *=     |
| Divide variable by value      | /=     |
| Bitwise-OR by value           | \|=    |
| Bitwise-AND by value          | &=     |
| Left-shift variable by value  | <<=    |
| Right-shift variable by value | >>=    |
| Increment variable by 1       | ++     |
| Decrement variable by 1       | --     |

### Relational (conditional)

| Operation                  | Symbol |
| -------------------------- | ------ |
| More than value            | >      |
| Less than value            | <      |
| More or equal to the value | >=     |
| Less or equal to the value | <=     |
| Equal                      | ==     |
| Not equal                  | !=     |

### Others

| Category   | Operator(s) |
| ---------- | ----------- |
| Logical    | &&, \|\|    |
| Bitwise    | &, \|, ~, ^ |
| String     | +           |
| Shift      | <<, >>      |
| Assignment | =           |
| Indexing   | [ ]         |
| Negation   | !           |

### Script Operator Overload

#### Index Operator

Overloading the index operator can be achieved through the Set and Get methods. You can have any number of overloads as long as there is no ambiguity with the types.

```cpp
class IndexOperatorTest
{
	int data[3];

	void Set(int _index, int _value)
	{
		data[_index] = _value;
	}

	int Get(int _index)
	{
		return data[_index];
	}
}

auto instance = new IndexOperatorTest();
instance[1] = 5;
Print(instance[1]); // prints '5'
```

> **Note:** If you want to assign a pre-parsed vector with instance[index] = "1 1 1" the setter needs to accept a string as _value parameter and convert it explicitly using _value.ToVector() before an assignment.

## Keywords

### Function/method modifiers

| Keyword   | Description                                                                                                                                                          |
| --------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| private   | The method can be called only from inside of the same class method                                                                                                   |
| protected | The method can be called only from inside of class method or methods of its extended classes                                                                         |
| static    | The method can be called without object pointer, just by className.methodName() , only static members of the same class can be accessed from inside of static method |
| override  | Compiler checks if is method present in base class and if method signature match                                                                                     |
| proto     | Prototyping of internal function (C++ side)                                                                                                                          |
| native    | Native call convention of internal function (C++ side)                                                                                                               |

### Variable modifiers

| Keyword   | Description                                                                                                                                                                      |
| --------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| private   | Variable can be accessed only from inside of class methods. Mutually exclusive with "protected"                                                                                  |
| protected | Variable can be accessed only from inside of class methods or methods of its extended classes. Mutually exclusive with "private"                                                 |
| static    | Variable can be accessed without object pointer, using className.variable                                                                                                        |
| autoptr   | Modifier for variables of class pointer type. Pointer target will be automatically destroyed upon end of variable lifetime (end of scope or deletion of class which contains it) |
| proto     | Prototyping of internal function (C++ side)                                                                                                                                      |
| ref       | Variable is a strong reference                                                                                                                                                   |
| const     | Constant, cannot be modified                                                                                                                                                     |
| out       | Modifier for function parameters, variable will be changed by a function call                                                                                                    |
| inout     | Modifier for function parameters, variable will be used and then changed by a function call                                                                                      |

### Class modifiers

| Keyword | Description                            |
| ------- | -------------------------------------- |
| modded  | Inheritance-like behaviour for modding |

### Other Keywords

| Keyword | Description                                                          |
| ------- | -------------------------------------------------------------------- |
| new     | Create new object instance                                           |
| delete  | Destroy object instance                                              |
| class   | Class declaration                                                    |
| extends | Class inheritence                                                    |
| typedef | Type definition                                                      |
| return  | Terminates function & returns value (if specified)                   |
| null    | null value                                                           |
| this    | Address of the object, on which the member function is being called  |
| super   | Refers to the base class for the requested variable/function         |
| thread  | Declared before the function call, runs the function on a new thread |

## Types

### Primitive Types

| Type name | Range                                 | Default Value     |
| --------- | ------------------------------------- | ----------------- |
| int       | from −2,147,483,648 to +2,147,483,647 | 0                 |
| float     | from ±1.401298E−45 to ±3.402823E+38   | 0.0               |
| bool      | true or false                         | false             |
| string    | -                                     | "" (empty string) |
| vector    | see float                             | (0.0,0.0,0.0)     |
| void      | -                                     | -                 |
| Class     | -                                     | null              |
| typename  | -                                     | null              |

### Strings

- Strings are passed by value, like primitive types
- Can be concatenated by + operator
- Strings are initialized and destroyed automatically
- Strings can contain standardized escape sequences. These are supported: \n \r \t \\ \"

```cpp
void Method()
{
	string a = "Hello";
	string b = " world!";
	string c = a + b;

	Print(a); // prints "Hello"
	Print(b); // prints " world!"
	Print(c); // prints "Hello world!"
}
```

### Vectors

- Vectors are passed by value, like primitive types
- Vector values are accessible by [, ] operator, like static arrays
- Vectors are initialized and destroyed automatically
- Vector can be initialized by three numeric values in double quotes e.g. "10 22 13"

```cpp
void Method()
{
	vector up = "0 1 0"; // initialized by values <0, 1, 0>
	vector down; // vector "down" has now default value <0, 0, 0>

	down = up;
	down[1] = -1; // change Y value of vector "down"

	Print(up); // prints <0, 1, 0>
	Print(down); // prints <0, -1, 0>
}
```

### Objects

- Objects in enforce script are references and are passed by reference
- All member functions and variables are public by default. Use 'private' keyword to make them private
- 'autoptr' keyword before object variable declaration ensures that compiler automatically destroys the object when the scope is terminated (e.g. function call ends)

```cpp
class MyClass
{
	void Say()
	{
		Print("Hello world");
	}

	void MethodA()
	{
		MyClass o; // o == null
		o = new MyClass; // creates a new instance of MyClass class
		o.Say(); // calls Say() function on instance 'o'
		delete o; // destroys 'o' instance
	}

	void MethodB()
	{
		// if you type autoptr into declaration, compiler automatically does "delete o;" when the scope is terminated
		autoptr MyClass o; // o == null
		o = new MyClass; // creates a new instance of MyClass class
		o.Say(); // calls Say() function on instance 'o'
	}

	void MethodC()
	{
		MyClass o;
		o = new MyClass;
		o.Say();
		// This function will delete the object if the reference count of the object is 1 when the function's scope is terminated
	}

	void UnsafeMethod(MyClass o) // Method not checking for existence of the input argument
	{
		o.Say();
	}

	void SafeMethod(MyClass o)
	{
		if (o)
		{
			o.Say();
		}
		else
		{
			Print("Hey! Object 'o' is not initialised!");
		}
	}

	void MethodD()
	{
		autoptr MyClass o;
		o = new MyClass;

		SafeMethod(o); // ok
		UnsafeMethod(o); // ok

		SafeMethod(null); // ok
		UnsafeMethod(null); // Crash! Object 'o' is not initialised and UnsafeMethod accessed it!
	}
}
```

**Example of this & super:**

```cpp
class AnimalClass
{
	void Hello()
	{
		Print("AnimalClass.Hello()");
	}
};

class HoneyBadger: AnimalClass
{
	override void Hello()
	{
		Print("HoneyBadger.Hello()");
	}

	void Test()
	{
		Hello(); // prints "HoneyBadger.Hello()"
		this.Hello(); // 'this' refers to this instance of object, so same as line above, prints "HoneyBadger.Hello()"
		super.Hello(); // refers to base(super) class members, prints "AnimalClass.Hello()"
	}
}
```

### Enums

Enumerators are set of named constant identifiers.

- enums have int type
- enum item value can be assigned in definition, otherwise it is computed automatically (previous item value plus one)
- enum can inherit from another enum (item value continues from last parent item value)
- enum name used as type behaves like ordinary int (no enum value checking on assign)

```cpp
enum MyEnumBase
{
	Alfa = 5,	// has value 5
	Beta,		// has value 6
	Gamma		// has value 7
};
enum MyEnum: MyEnumBase
{
	Blue,			// has value 8
	Yellow,			// has value 9
	Green = 20,		// has value 20
	Orange			// has value 21
};

void Test()
{
	int a = MyEnum.Beta;
	MyEnum b = MyEnum.Green;
	int c = b;

	Print(a); // prints '6'
	Print(b); // prints '20'
	Print(c); // prints '20'

	Print(MyEnum.Alfa); // prints '5'
}
```

### Templates

Enforce Script has template feature similar to C++ Templates, which allows classes to operate with generic types.

- Generic type declaration is placed inside <, > (e.g. "class TemplateClass<class GenericType>" )operators after template class name identifier
- Enforce Script supports any number of generic types per template class

```cpp
class Item<Class T>
{
	T m_data;
	void Item(T data)
	{
		m_data = data;
	}
	void SetData(T data)
	{
		m_data = data;
	}
	T GetData()
	{
		return m_data;
	}
	void PrintData()
	{
		Print(m_data);
	}
};

void Method()
{
	Item<string> string_item = new Item<string>("Hello!"); // template class Item declared with type "string". In Item<string> class, all "T"s are substituted with 'string'
	Item<int> int_item = new Item<int>(72); // template class Item declared with type "int". In Item<int> class, all "T"s are substituted with 'int'

	string_item.PrintData(); // prints "m_data = 'Hello!'"
	int_item.PrintData(); // prints "m_data = 72"
}
```

### Arrays

#### Static Arrays

- Arrays are indexed from 0
- Arrays are passed by reference, like objects
- Static arrays are initialized and destroyed automatically
- Size of static arrays can be set only during compilation time
- Elements are accessible by array access operator [ ]

```cpp
void MethodA()
{
	int numbersArray[3]; // declaring array of int with size 3

	numbersArray[0] = 54;
	numbersArray[1] = 82;
	numbersArray[2] = 7;

	int anotherArray[3] = {53, 90, 7};
 }

const int ARRAY_SIZE = 5;

void MethodB()
{
	int numbersArray[ARRAY_SIZE]; // declaring array of int with size of value of ARRAY_SIZE constant

	numbersArray[0] = 54;
	numbersArray[1] = 82;
	numbersArray[2] = 7;
	numbersArray[3] = 1000;
	numbersArray[4] = 324;
}

void MethodC()
{
	int size = 3;
	int numbersArray[size]; // err! size static array cannot be declared by variable!
}
```

#### Dynamic Arrays

- Dynamic arrays support change of size at runtime by inserting/removing array items
- Dynamic arrays are provided through 'array' template class
- Dynamic arrays are passed by reference
- Dynamic Arrays are objects and therefore they must be created and destroyed like objects, so don't forget to use "autoptr" or delete operator!
- Elements are accessible by "Get" function or by array access operator [ ]
- There are already defined typedefs for primitive type arrays:
  - array<string> = TStringArray
  - array<float> = TFloatArray
  - array<int> = TIntArray
  - array<class> = TClassArray
  - array<vector> = TVectorArray

```cpp
void Method()
{
	autoptr TStringArray nameArray = new TStringArray; // dynamic array declaration, "TStringArray" is the same as "array<string>"

	nameArray.Insert("Peter");
	nameArray.Insert("Michal");
	nameArray.Insert("David");

	string name;
	name = nameArray.Get(1); // gets second element of array "nameArray"
	Print(name); // prints "name = 'Michal'"

	nameArray.Remove(1); // second element is removed
	name = nameArray.Get(1); // gets second element of array "nameArray"
	Print(name); // prints "name = 'David'"

	int nameCount = nameArray.Count(); // gets elements count of array "nameArray"
	Print(nameCount); // prints "nameCount = 2"
}
```

### Automatic type detection

The variable type will be detected automatically at compile time when the keyword auto is used as placeholder.

```cpp
class MyCustomClass(){}

void Method()
{
	auto variableName = 1; // variableName will be of type integer
	auto variablePi = 3.14; // variablePi will be of type float
	auto variableInst = new MyCustomClass(); // variableInst will be of type MyCustomClass
}
```

## Control Structures

Control structures work very similar to C# or C/C++ languages.

### Conditional structures

#### If statement

```cpp
void Method()
{
	int a = 4;
	int b = 5;

	if (a > 0)
	{
		Print("A is greater than zero!");
	}
	else
	{
		Print("A is not greater than zero!");
	}

	if (a > 0 && b > 0)
	{
		Print("A and B are greater than zero!");
	}

	if (a > 0 || b > 0)
	{
		Print("A or B are greater than zero!");
	}

	// 'else if' example
	if (a > 10)
	{
		Print("a is bigger then 10");
	}
	else if (a > 5)
	{
		Print("a is bigger then 5 but smaller than 10");
	}
	else
	{
		Print("a is smaller then 5");
	}
}
```

#### Switch statement

Switch statement supports switching by numbers, constants and strings.

```cpp
void Method()
{
	int a = 2;

	switch(a)
	{
		case 1:
			Print("a is 1");
			break;

		case 2:
			Print("a is 2"); // this one is called
			break;

		default:
			Print("it's something else");
			break;
	}

	// using switch with constants
	const int LOW = 0;
	const int MEDIUM = 1;
	const int HIGH = 2;

	int quality = MEDIUM;
	switch(quality)
	{
		case LOW:
			// do something
			break;

		case MEDIUM:
			// this one is called
			// do something
			break;

		case HIGH:
			// do something
			break;
	}

	// using switch with strings
	string name = "peter";
	switch(name)
	{
		case "john":
			Print("Hello John!");
			break;

		case "michal":
			Print("Hello Michal!");
			break;

		case "peter":
			Print("Hello Peter!"); // this one is called
			break;
	}
}
```

### Iteration structures

#### For

The for loop consists of three parts: declaration, condition and increment.

```cpp
void Method()
{
	// this code prints
	// "i = 0"
	// "i = 1"
	// "i = 2"
	for (int i = 0; i < 3; i++)
	{
		Print(i);
	}
}

// this function print all elements from dynamic array of strings
void ListArray(TStringArray a)
{
	if (a == null) // check if "a" is not null
		return;

	int i = 0;
	int c = a.Count();

	for (i = 0; i < c; i++)
	{
		string tmp = a.Get(i);
		Print(tmp);
	}
}
```

#### Foreach

Simpler and more comfortable version of for loop.

```cpp
void TestFn()
{
	int pole1[] = {7,3,6,8};
	array<string> pole2 = {"a", "b", "c"};
	auto mapa = new map<string, int>();
	mapa["jan"] = 1;
	mapa["feb"] = 2;
	mapa["mar"] = 3;

	// simple foreach iteration
	foreach(int v: pole1) // prints: '7', '3', '6', '8'
	{
		Print(v);
	}

	// foreach iteration with key (if you iterate trough array, key is filled with array index)
	foreach(int i, string j: pole2) // prints: 'pole[0] = a', 'pole[1] = b', 'pole[2] = c'
	{
		Print("pole[" + i + "] = " + j);
	}

	// map iteration, with key and value
	foreach(auto k, auto a: mapa) // prints: 'mapa[jan] = 1', 'mapa[feb] = 2', 'mapa[mar] = 3'
	{
		Print("mapa[" + k + "] = " + a);
	}

	// map iteration with just value
	foreach(auto b: mapa) // prints: '1', '2', '3'
	{
		Print(b);
	}
}
```

#### While

```cpp
void Method()
{
	int i = 0;

	// this code prints
	// "i = 0"
	// "i = 1"
	// "i = 2"

	while (i < 3)
	{
		Print(i);
		i++;
	}
}
```

## Object-oriented programming specifics

- All member functions and variables are public by default. You can use 'private' or 'protected' keyword to control access
- Class member functions are virtual and can be overridden by child class
- Use override keyword for overriding base class methods(to avoid accidental overriding)
- Class can inherit from one parent class using keyword 'extends'
- Objects are not initialized and destroyed automatically, use 'new' and 'delete' (or 'autoptr' feature)
- Class variables are cleared to default values upon creation

### Inheritance

```cpp
class AnimalClass
{
	void MakeSound()
	{
	}
};

class Dog: AnimalClass
{
	override void MakeSound()
	{
		Print("Wof! Wof!");
	}

	void Aport()
	{
		// do something
	}
};

class Cat: AnimalClass
{
	override void MakeSound()
	{
		Print("Meow!");
	}

	void Scratch()
	{
		// do something
	}
};

void LetAnimalMakeSound(AnimalClass pet)
{
	if (pet) // check if pet is not null
	{
		pet.MakeSound();
	}
}

void Method()
{
	Cat nyan = new Cat;
	Dog pluto = new Dog;

	nyan.MakeSound(); // prints "Meow!"
	pluto.MakeSound(); // prints "Wof! Wof!"

	LetAnimalMakeSound(nyan); // prints "Meow!"
	LetAnimalMakeSound(pluto); // prints "Wof! Wof!"
}
```

### Constructor & Destructor

Constructor and destructor are special member functions

- Every class can have one constructor and one destructor
- Constructor is function called when object is created(by 'new') and has same name as class ( e.g. 'void ClassName()' )
- Destructor is called when object is going to be destroyed (by 'delete'/'autoptr') and has same name as class with tilde character at beginning ( e.g. 'void ~ClassName()' )
- Constructor can have initialization parameters, destructor cannot have any parameters
- Both constructor and destructor do not return any value (returns void)
- Constructor and destructor are called even when object is created or destroyed from C++
- When constructor doesn't have any parameters omit brackets while using 'new' operator

```cpp
class MyClassA
{
	void MyClassA() // constructor declaration of class MyClassA
	{
		Print("Instance of MyClassA is created!");
	}

	void ~MyClassA() // destructor declaration of class MyClassA
	{
		Print("Instance of MyClassA is destroyed!");
	}
};

class MyClassB
{
	string m_name;

	void MyClassB(string name) // constructor declaration of class MyClassB
	{
		m_name = name;
		Print("Instance of MyClassB is created!");
	}

	void ~MyClassB() // destructor declaration of class MyClassB
	{
		Print("Instance of MyClassB is destroyed!");
	}
};

void Method()
{
	MyClassA a = new MyClassA; // prints "Instance of MyClassA is created!"
	MyClassB b = new MyClassB("Michal"); // prints "Instance of MyClassB is created!"

	delete b; // prints "Instance of MyClassB is destroyed!"
} // here at the end of scope ARC feature automatically destroys 'a' object and it prints "Instance of MyClassA is destroyed!"
```

### Managed class & pointer safety

Since script does not do garbage collecting automatically, all plain pointers are considered unsafe

- All classes inherited from Managed class work soft links instead of plain pointers. Soft link is weak reference that does not keep the object alive and is zeroed upon their destruction so they are never invalid
- All objects available in game module should be Managed, so they should be using soft links by default (they all inherits from Managed class)

```cpp
// without Managed
class A
{
	void Hello()
	{
		Print("hello");
	}
}

void TestA()
{
	A a1 = new A();
	A a2 = a1; // both a2 and a1 contain pointer to the same instance of A

	a1.Hello(); // prints "hello"
	a2.Hello(); // prints "hello"
	delete a1; // our instance of A is deleted and a1 is set to NULL

	if (a1) a1.Hello(); // nothing happens because a1 is NULL
	if (a2) a2.Hello(); // a2 is still pointing to deleted instance of A so condition pass. This line cause crash!
}
```

```cpp
// with Managed
class B: Managed
{
	void Hello()
	{
		Print("hello");
	}
}

void TestB()
{
	B a1 = new B();
	B a2 = a1; // both a2 and a1 contain pointer to the same instance of B

	a1.Hello(); // prints "hello"
	a2.Hello(); // prints "hello"
	delete a1; // our instance of B is deleted and a1 is set to NULL, thanks to Managed(soft links) a2 (and all other possible pointers) is also NULL

	if (a1) a1.Hello(); // nothing happens because a1 is NULL
	if (a2) a2.Hello(); // nothing happens because a2 is also NULL, thus this code will always be safe
}
```

### Automatic Reference Counting

Enforce Script has support of automatic reference counting. In a spirit of flexibility, you can choose if your class should or shouldn't be managed, by choosing to inherit from Managed class.

**Simple "C++" like classes** remains an option for high performance, but less secure scripts

- objects referenced by plain C pointers
- no automatic memory management, owner must delete them

**For common gameplay scripts/mods/etc** there are managed objects, which are

- slightly slower (due indirect weak pointer object accessing)
- internally ref-counted and automatically deleted when not needed
- objects referenced by weak pointers (pointer is always valid or NULL, never points to deleted memory etc.)

#### Strong and weak references

- **strong reference** increases reference count - holds object alive
- **weak reference** just pointing to an object, but doesn't increase reference count

```cpp
class Parent
{
	ref Child m_child; // putting 'ref' keyword, we give a hint to compiler, that this is strong reference.
};

class Child
{
	Parent m_parent; // this reference is weak reference (there is no 'ref')
};

void main()
{
	Parent a = new Parent(); // 'a' has 1 strong reference (local vars are strong by default)
	Child b = new Child(); // 'b' has 1 reference (local vars are strong by default)
	a.m_child = b; // 'b' has 2 strong references, because Parent.m_child is strong ref
	b.m_parent = a; // 'a' has still just 1 strong reference, because Child.m_parent is weak reference

	// local variables 'a', 'b' are released (reference count is decreased by 1)
}
```

In the code above at the end of function main, object 'a' has zero strong references thus is deleted, destructor releases m_child, and so the object 'b' also has zero strong references and it is deleted.

#### Usage of ref keyword

In the Enforce script by default all variables are weak references, ref keyword is marking that variable is strong reference. In some special cases, variables are strong references by default

- local variables inside functions
- function arguments
- function return value

and are released after their scope ends.

While an object is stored in at least one strong reference, it's being kept alive. When the last strong reference is destroyed or overwritten, the object is destroyed and all other (only weak refs left) references are set to NULL. When an object is deleted manually by delete command (e.g., 'delete a;'), it is deleted immediately ignoring reference count, and all references (weak and strong) are set to NULL.

**Optimal usage of references** in Enforce script is to have exactly one strong reference per object, placed in "owner" object who creates it. This way of usage ensures

- no cyclic references
- proper order of object destruction - object is destroyed when its "creator" is destroyed

**Examples:**

```cpp
class MyClassA
{
	void MyClassA() { Print("MyClassA()"); } // constructor
	void ~MyClassA() { Print("~MyClassA()"); } // destructor
};

void function1()
{
	MyClassA a = new MyClassA(); // 'MyClassA()'
	a = new MyClassA();
	// new object is created 'MyClassA()',
	// 'a' is overwritten,
	// first instance of MyClass is released: '~MyClassA()'
	// any code here

	// the end of function, local variable 'a' is released: '~MyClassA()'
}

ref MyClassA g_MyGlobal;

void function2()
{
	MyClassA a = new MyClassA(); // 'MyClassA()'
	g_MyGlobal = a;

	// the end of function, local variable 'a' is released, but there is still one strong ref g_MyGlobal,
	// so the object will live until the end of this script module.
}

void PrintMyObj(MyClassA o)
{
	Print("object is " + o);
}

void function3()
{
	MyClassA a = new MyClassA(); // 'MyClassA()'
	PrintMyObj(a); // 'object is MyClassA<address>'

	// the end of function, local variable 'a' is released: '~MyClassA()'
}

void function4()
{
	PrintMyObj(new MyClassA());
	// object is created 'MyClassA()',
	// PrintMyObj function is called 'object is MyClassA<address>'
	//and right after PrintMyObj function finishes, temporary object is released '~MyClassA()'
}

MyClassA  CreateMyObj()
{
	return new MyClassA();
}

void function5()
{
	MyClassA a = CreateMyObj(); // object is created inside CreateMyObj function 'MyClassA()'
	// any code here

	// the end of function, local variable 'a' is released: '~MyClassA()'
}

void function6()
{
	array<MyClassA> pole1 = new array<MyClassA>(); // array of weak references is created
	pole1.Insert(new MyClassA());
	// object is created 'MyClassA()',
	// but pole1 is array of weak references, so no one is keeping this object alive,
	// its deleted immediately '~MyClassA()'

	array<ref MyClassA> pole2 = new array<ref MyClassA>(); // array of strong references is created
	pole2.Insert(new MyClassA()); // object is created 'MyClassA()'

	// any code here

	// the end of function, local variables 'pole1' and 'pole2' are released:
	// pole1 is destroyed (it contains just NULL),
	// pole2 is destroyed (it contains last strong ref to object, so object is destroyed '~MyClassA()')
}

// classes and arrays
class ExampleClass
{
	//! weak reference to object
	MyClassA m_a;

	//! strong reference to object
	ref MyClassA m_b;

	//! strong reference to dynami array(dynamic array is object itself) of weak references
	ref array<MyClassA> m_c;

	//! strong reference to dynamic array of strong references
	ref array<ref MyClassA> m_d;

	//! static array of strong references
	ref MyClassA m_e[10];
};
```

## Modding

### Modded class

Modded class is used to inject inherited class into class hierarchy without modifying other scripts, which is required for proper modding:

- Modded class behaves like class inherited from original class (you can use super to access original class)
- When modded class is declared, it will be instanced instead of original class everywhere in the script
- When several modded classes are modding the same vanilla class, the next modded class will instead inherit of the latest modded class, which enables mod compatibility

```cpp
// original
class ModMe
{
	void Say()
	{
		Print("Hello original");
	}
};

// First mod
modded class ModMe  // this class automatically inherits from original class ModMe
{
	override void Say()
	{
		Print("Hello modded One");
		super.Say();
	}
};

// Second mod
modded class ModMe  // this class automatically inherits from first mod's ModMe
{
	override void Say()
	{
		Print("Hello modded Two");
		super.Say();
	}
};

void Test()
{
	ModMe a = new ModMe(); // modded class ModMe is instanced
	a.Say(); // prints 'Hello modded Two' , 'Hello modded One' and 'Hello original'
}
```

### Modded constants

Constants can be overridden on compilation by the the last loaded mod (be mindful of the mod load order)

- Allows multiple mods to change different constants in a single class

```cpp
class BaseTest
{
	const int CONST_BASE = 4;
}

class TestConst: BaseTest
{
	const int CONST_TEST = 5;
}

modded class TestConst
{
	const int CONST_BASE = 1;
	const int CONST_TEST = 2;
	const int CONST_MOD = 3;
}

void TestPrint()
{
	Print(TestConst.CONST_BASE); // 1
	Print(TestConst.CONST_TEST); // 2
	Print(TestConst.CONST_MOD);  // 3
}
```

### Modded private members

Even though modded class behaves similar to an inherited one, it can still access private members of the vanilla class

```cpp
// original
class VanillaClass
{
	private bool imPrivate = 0;

	private void DoSomething()
	{
		Print("Vanilla method");
	}
}
```

```cpp
// accesss
modded class VanillaClass
{
	void AccessPvt()
	{
		Print(imPrivate);
		DoSomething();
	}
}
```

```cpp
// override
modded class VanillaClass
{
	override void DoSomething()
	{
		super.DoSomething();
		Print("Modded method");
	}
}
```
