use std::{collections::HashMap, vec, char};

static TOKEN_BREAKERS: &[char] = &['{', '}', '(', ')', '=', ';', ' ', '[', ']'];

#[derive(Debug, PartialEq, Clone)]
enum Type {
    Int,
    Bool,
    Char,
    Error,
    Function(Box<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Int(String),
    Bool(String),
    Char(String),
}

#[derive(Debug)]
enum Term {
    Var(&'static str),
    Int(i32),
    Bool(bool),
    Char(char),
    FunctionCall(Box<Term>, Box<Term>),
    TermAbstraction(Box<Term>, Type, Box<Term>),
}

#[allow(dead_code)]
fn main() {
    let my_var = Term::Var("x");
    dbg!(&my_var);

    let name = 500;
    println!("{:X}", name as usize);

    let name = 500;
    println!("{:X}", name as usize);

    // tipos
    // function f(char $x): char { return $x }
    let tipo_da_minha_funcao = Type::Function(Box::new(Type::Char), Box::new(Type::Char));

    // function f(int $x): bool { return $x & 1 === 0 }
    let outro_tipo = Type::Function(Box::new(Type::Int), Box::new(Type::Bool));

    // function f(int $x): fn(int) -> int {
    let poggers = Type::Function(
        Box::new(Type::Int),
        Box::new(Type::Function(Box::new(Type::Int), Box::new(Type::Int))),
    );

    // terms
    // function f(int $x): int { return $x }
    let random_value: Term = Term::TermAbstraction(
        Box::new(Term::Var("x")),
        Type::Int,
        Box::new(Term::Var("x")),
    );

    // function f(fn(int): int $g): int { return $g(2) }

    let sexo: Term = Term::TermAbstraction(
        Box::new(Term::Var("g")),
        Type::Function(Box::new(Type::Int), Box::new(Type::Int)),
        Box::new(Term::FunctionCall(
            Box::new(Term::Var("g")),
            Box::new(Term::Int(2)),
        )),
    );

    let mut map = HashMap::new();

    map.insert("fodase", Type::Int);

    // λ x : int -> x
    let term_type = infer(
        Term::TermAbstraction(
            Box::new(Term::Var("x")),
            Type::Int,
            Box::new(Term::Var("x"))
        ),
        map
    );

    println!("{:?}", term_type);
}

fn infer(term: Term, mut map: HashMap<&str, Type>) -> Type {
    match term {
        Term::Int(_) => Type::Int,
        Term::Char(_) => Type::Char,
        Term::Bool(_) => Type::Bool,
        Term::Var(param) => map.get(param).unwrap().clone(),
        Term::TermAbstraction(param, param_type, result) => {
            let param = match *param {
                Term::Var(poggers) => poggers,
                _ => panic!("qq ce ta fazendo cara..."),
            };

            map.insert(param, param_type.clone());

            Type::Function(Box::new(param_type), Box::new(infer(*result, map)))
        }
        Term::FunctionCall(term_abstraction, term) => {
            let argument_type = infer(*term, map.clone());

            match infer(*term_abstraction, map) {
                Type::Function(param_type, return_type) => {
                    if *param_type != argument_type {
                        return Type::Error
                    }
                    *return_type
                }
                _ => Type::Error
            }
        }
    }
}

fn lex(value: &str) -> Vec<Token> {
    let mut tokens = vec![];

    let value = value.to_string();

    let value = value.chars().collect::<Vec<_>>();

    let mut index = 0;

    loop {
        if index >= value.len() {
            break
        };

        let current_char = value[index];

        if current_char.is_numeric() {
            let token = concat_until(
                &value[index..],
                |char| { !char.is_numeric() }
            );

            index += token.len();

            tokens.push(Token::Int(token));
        };

        match current_char {
            't' => {
                let token = get_current_token(&value[index..]);

                if token.eq("true") {
                    tokens.push(Token::Bool(token.clone()));

                    index += token.len();
                }
            },
            'f' => {
                let token = get_current_token(&value[index..]);

                if token.eq("false") {
                    tokens.push(Token::Bool(token.clone()));

                    index += token.len();
                }
            },
            '\'' => {
                let token = concat_until(
                    &value[index + 1 ..],
                    |char| { char == '\'' }
                );

                println!("{:?}", token);

                match token.len().cmp(&1) {
                    std::cmp::Ordering::Equal => {
                        tokens.push(Token::Char(token.clone()));
                    },
                    std::cmp::Ordering::Less => {
                        panic!("Char must not be empty");
                    },
                    std::cmp::Ordering::Greater => {
                        panic!("Char must be one char long");
                    },
                };

                index += token.len() + 1;
            },
            _ => {}
        };

        index += 1;
    };

    tokens
}

fn get_current_token(chars: &[char]) -> String {
    concat_until(
        chars,
        |char| {
            TOKEN_BREAKERS.contains(&char)
        }
    )
}

fn concat_until(chars: &[char], condition: fn(char) -> bool) -> String {
    let mut result = String::from("");

    for char in chars.iter() {
        match condition(*char) {
            false => { result.push(*char) }
            true => { break },
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_int() {
        assert_eq!(Type::Int, infer(Term::Int(1), HashMap::new()));
    }

    #[test]
    fn simple_bool() {
        assert_eq!(Type::Bool, infer(Term::Bool(true), HashMap::new()));
    }

    #[test]
    fn simple_char() {
        assert_eq!(Type::Char, infer(Term::Char('a'), HashMap::new()));
    }

    #[test]
    fn simple_function() {
        assert_eq!(
            Type::Function(Box::new(Type::Int), Box::new(Type::Int)),
            // λx: int -> x
            infer(
                Term::TermAbstraction(
                    Box::new(Term::Var("x")),
                    Type::Int,
                    Box::new(Term::Var("x"))
                ),
                HashMap::new()
            )
        )
    }

    #[ignore]
    #[test]
    fn function_as_function_param() {
        assert_eq!(
            Type::Function(
                Box::new(Type::Function(
                    Box::new(Type::Int),
                    Box::new(Type::Int)
                )),
                Box::new(Type::Function(Box::new(Type::Int),Box::new(Type::Int)))
            ),
            // λ(λy: int -> y): int -> x
            infer(
                Term::TermAbstraction(
                    Box::new(Term::TermAbstraction(
                        Box::new(Term::Var("y")),
                        Type::Int,
                        Box::new(Term::Var("y"))
                    )),
                    Type::Function(Box::new(Type::Int), Box::new(Type::Int)),
                    Box::new(Term::Var("x"))
                ),
                HashMap::new()
            )
        )
    }

    #[test]
    fn function_call(){
      assert_eq!(
        // (λx: int . true) 10
        infer(Term::FunctionCall(
          Box::new(Term::TermAbstraction(
            Box::new(Term::Var("x")),
            Type::Int,
            Box::new(Term::Bool(true))
          )),
          Box::new(Term::Int(10))
        ),
          HashMap::new()
        ),
        Type::Bool
      );
    }

    #[test]
    fn function_call_with_invalid_argument_type(){
      assert_eq!(
        // (λx: int . true) false
        infer(Term::FunctionCall(
          Box::new(Term::TermAbstraction(
            Box::new(Term::Var("x")),
            Type::Int,
            Box::new(Term::Bool(true))
          )),
          Box::new(Term::Bool(false))
        ),
          HashMap::new()
        ),
        Type::Error
      );
    }

    #[test]
    fn applying_on_something_thats_not_an_abstraction(){
      assert_eq!(
        // 1 false
        infer(Term::FunctionCall(
          Box::new(Term::Int(1)),
          Box::new(Term::Bool(false))
        ),
          HashMap::new()
        ),
        Type::Error
      );
    }

    #[test]
    fn returns_a_token_from_string_with_breaker_character() {
        assert_eq!(
            "multiple".to_string(),
            get_current_token(&"multiple words".chars().collect::<Vec<char>>()[..])
        );
        assert_eq!(
            "semicolon".to_string(),
            get_current_token(&"semicolon;".chars().collect::<Vec<char>>()[..])
        );
    }

    #[test]
    fn lex_int() {
        assert_eq!(vec![Token::Int("1".to_string())], lex("1"));
        assert_eq!(vec![Token::Int("22".to_string())], lex("22"));
    }

    #[test]
    fn lex_bool() {
        assert_eq!(vec![Token::Bool("true".to_string())], lex("true"));
        assert_eq!(vec![Token::Bool("false".to_string())], lex("false"));
    }

    #[test]
    fn lex_char() {
        assert_eq!(
            vec![Token::Char("a".to_string())],
            lex("'a'")
        );
    }

    #[test]
    #[should_panic]
    fn lex_multiple_chars_into_quotes() {
        lex("'aa'");
    }

    #[test]
    fn lex_everything_together() {
        assert_eq!(
            vec![
                Token::Int("55".to_string()),
                Token::Bool("true".to_string()),
                Token::Char("a".to_string())
            ],
            lex("55 true 'a'")
        );
    }

    // #[test]
    // fn parse_int() {
    //   assert_eq!(Term::Int(1), parse("1"));
    // }
}
