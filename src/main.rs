use rosc::encoder::encode;
use rosc::OscMessage;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io;
use std::net::IpAddr;
use std::net::SocketAddr;
use std::net::UdpSocket;
use std::thread;
use std::time::Duration;

fn main() -> io::Result<()> {
    // Get the command line arguments
    let args: Vec<String> = env::args().collect();

    // Check if the user provided three arguments (program name, bind address, and connect address)
    if args.len() != 4 {
        eprintln!("Usage: <file> <bind_address> <connect_address>");
        std::process::exit(1);
    }

    // Create a UDP socket and bind it to the specified bind address
    let udp_socket = UdpSocket::bind(&args[2])?;
    let _ = udp_socket.connect(&args[3]).expect("could not connect.");

    // Read a file into a string
    let file_contents = fs::read_to_string(&args[1])?;

    let (lines, _) = parser(&Token::tokenize(&file_contents).unwrap()).unwrap();

    let mut wait = Duration::from_secs_f32(0.25);
    for line in lines {
        for statement in line {
            match statement {
                Statement::Assignment(name, value) => {
                    if name == "LPM" {
                        match value {
                            VarValue::OscType(OscType::Int(int)) => {
                                wait = Duration::from_secs_f64((int as f64 / 60.0).recip())
                            }
                            _ => eprintln!("LMP set to non-intiger. Not changing"),
                        }
                    }
                }
                Statement::Message(addr, args) => {
                    udp_socket.send(
                        &encode(&rosc::OscPacket::Message(msg2real_msg(addr, &args))).unwrap(),
                    )?;
                }
            }
        }
        thread::sleep(wait);
    }

    Ok(())
}
#[derive(Debug, Clone)]
enum Token {
    Addr(String),
    Int(i32),
    Float(f32),
    OscString(String),
    OscBlob(Vec<u8>),
    Var(String),
    Comma,
    Eq,
}

impl Token {
    fn tokenize(text: &str) -> Result<Vec<Vec<Token>>, TokenErr> {
        let lines = text.lines();
        let mut lines_tokenized = vec![];

        for (_line_n, line) in lines.enumerate() {
            let mut line_tokenized: Vec<Token> = vec![];
            let mut chars = line.char_indices();

            while let Some((i, chr)) = chars.next() {
                match chr {
                    ' ' => (),
                    ';' => {
                        chars.next();
                        break;
                    }
                    ',' => line_tokenized.push(Token::Comma),
                    '0'..='9' => {
                        let mut chrs = vec![chr];
                        let mut points = 0;
                        while let Some((i, chr)) = chars.next() {
                            match chr {
                                '0'..='9' => chrs.push(chr),
                                '.' => {
                                    chrs.push(chr);
                                    points += 1;
                                }
                                ' ' => break,
                                _ => return Err(TokenErr::InvalidCharacter(chr)),
                            }
                        }
                        if points > 1 {
                            return Err(TokenErr::TooManyPoints);
                        } else if points == 1 {
                            line_tokenized.push(Token::Float(
                                chrs.into_iter().collect::<String>().parse().unwrap(),
                            ))
                        } else if points == 0 {
                            line_tokenized.push(Token::Int(
                                chrs.into_iter().collect::<String>().parse().unwrap(),
                            ))
                        }
                    }
                    '=' => line_tokenized.push(Token::Eq),
                    '/' => {
                        let mut chrs = vec![chr];
                        while let Some((_, chr)) = chars.next() {
                            if chr == ' ' {
                                break;
                            } else {
                                chrs.push(chr)
                            }
                        }
                        line_tokenized.push(Token::Addr(chrs.into_iter().collect()))
                    }
                    '"' => {
                        let mut chrs = vec![chr];
                        loop {
                            match chars.next() {
                                Some((_, '\\')) => {
                                    match chars.next() {
                                        Some((_, 'n')) => chrs.push('\n'),
                                        Some((_, 't')) => chrs.push('\t'),
                                        Some((_, 'r')) => chrs.push('\r'),
                                        Some((_, '"')) => chrs.push('"'),
                                        Some((_, '\\')) => chrs.push('\\'),
                                        Some((_, _)) => return Err(TokenErr::UnknownEscape),
                                        None => return Err(TokenErr::StringNotFinished),
                                    }
                                    chrs.push(chr);
                                }
                                Some((_, '"')) => break,
                                Some((_, n)) => chrs.push(n),
                                None => return Err(TokenErr::StringNotFinished),
                            }
                        }

                        line_tokenized.push(Token::OscString(chrs.into_iter().collect()))
                    }
                    '#' => {
                        let mut chrs = vec![];
                        while let Some((_, chr)) = chars.next() {
                            match chr {
                                ' ' => break,
                                '_' => (),
                                '0'..='9' | 'a'..='f' | 'A'..='F' => chrs.push(chr),
                                _ => return Err(TokenErr::InvalidCharacter(chr)),
                            }
                        }
                        println!("{:?}", chrs);
                        if chrs.len() % 2 == 0 {
                            let mut bytes = vec![];
                            let mut last_byte: u8 = 0;
                            for i in 0..chrs.len() {
                                if i % 2 == 0 {
                                    last_byte = match chrs[i] {
                                        '0' => 0,
                                        '1' => 1,
                                        '2' => 2,
                                        '3' => 3,
                                        '4' => 4,
                                        '5' => 5,
                                        '6' => 6,
                                        '7' => 7,
                                        '8' => 8,
                                        '9' => 9,
                                        'a' => 10,
                                        'b' => 11,
                                        'c' => 12,
                                        'd' => 13,
                                        'e' => 14,
                                        'f' => 15,
                                        _ => unreachable!(),
                                    }
                                } else {
                                    last_byte += match chrs[i] {
                                        '0' => 0u8,
                                        '1' => 1,
                                        '2' => 2,
                                        '3' => 3,
                                        '4' => 4,
                                        '5' => 5,
                                        '6' => 6,
                                        '7' => 7,
                                        '8' => 8,
                                        '9' => 9,
                                        'a' => 10,
                                        'b' => 11,
                                        'c' => 12,
                                        'd' => 13,
                                        'e' => 14,
                                        'f' => 15,
                                        _ => unreachable!(),
                                    }
                                    .pow(2);
                                    bytes.push(last_byte);
                                }
                            }
                            line_tokenized.push(Token::OscBlob(bytes))
                        } else {
                            return Err(TokenErr::UnfinishedByte);
                        }
                    }
                    _ => {
                        let mut chrs = vec![chr];
                        while let Some((_, chr)) = chars.next() {
                            if chr != ' ' {
                                chrs.push(chr)
                            } else {
                                break;
                            }
                        }
                        line_tokenized.push(Token::Var(chrs.into_iter().collect()))
                    }
                }
            }
            lines_tokenized.push(line_tokenized)
        }
        return Ok(lines_tokenized);
    }
}

#[derive(Debug, Clone, Copy)]
enum TokenErr {
    TooManyPoints,
    InvalidCharacter(char),
    StringNotFinished,
    UnknownEscape,
    UnfinishedByte,
}

#[derive(Clone, Debug)]
enum VarValue {
    Addr(String),
    OscType(OscType),
}

#[derive(Clone, Debug)]
enum OscType {
    Int(i32),
    Float(f32),
    OscString(String),
    OscBlob(Vec<u8>),
}

#[derive(Debug)]
enum Statement {
    Assignment(String, VarValue),
    Message(String, Vec<OscType>),
}

fn parser(
    tokens: &[Vec<Token>],
) -> Result<(Vec<Vec<Statement>>, HashMap<String, VarValue>), ParseError> {
    let mut statements = Vec::new();
    let mut vars: HashMap<String, VarValue> = HashMap::new();
    for (line_n, line) in tokens.iter().enumerate() {
        let mut line = line.iter().enumerate();
        let mut line_statements = Vec::new();

        while let Some((i, token)) = line.next() {
            match token {
                Token::Addr(addr) => {
                    let mut args = Vec::new();
                    while let Some((i, token)) = line.next() {
                        match token {
                            Token::Addr(_) | Token::Eq => {
                                return Err(ParseError {
                                    line: line_n,
                                    token: i,
                                    kind: ParseErrorKind::UnexpectedToken,
                                })
                            }
                            Token::Var(var) => match vars.get(var) {
                                None => {
                                    return Err(ParseError {
                                        line: line_n,
                                        token: i,
                                        kind: ParseErrorKind::UnknownVariable,
                                    })
                                }
                                Some(VarValue::Addr(s)) => {
                                    args.push(OscType::OscString(s.clone()));
                                }
                                Some(VarValue::OscType(t)) => {
                                    args.push(t.clone());
                                }
                            },
                            Token::Int(i) => args.push(OscType::Int(*i)),
                            Token::Float(f) => args.push(OscType::Float(*f)),
                            Token::OscString(s) => args.push(OscType::OscString(s.clone())),
                            Token::OscBlob(b) => args.push(OscType::OscBlob(b.clone())),
                            Token::Comma => break,
                        }
                    }
                    line_statements.push(Statement::Message(addr.clone(), args))
                }
                Token::Var(var_name) => {
                    match line.next() {
                        Some((_, Token::Eq)) => match line.next() {
                            Some((_, Token::Addr(addr))) => {
                                line_statements.push(Statement::Assignment(
                                    var_name.clone(),
                                    VarValue::Addr(addr.clone()),
                                ));
                                vars.insert(var_name.clone(), VarValue::Addr(addr.clone()));
                            }
                            Some((_, Token::Float(f))) => {
                                line_statements.push(Statement::Assignment(
                                    var_name.clone(),
                                    VarValue::OscType(OscType::Float(*f)),
                                ));

                                vars.insert(
                                    var_name.clone(),
                                    VarValue::OscType(OscType::Float(*f)),
                                );
                            }
                            Some((_, Token::Int(i))) => {
                                vars.insert(var_name.clone(), VarValue::OscType(OscType::Int(*i)));
                                line_statements.push(Statement::Assignment(
                                    var_name.clone(),
                                    VarValue::OscType(OscType::Int(*i)),
                                ));
                            }
                            Some((_, Token::OscBlob(b))) => {
                                line_statements.push(Statement::Assignment(
                                    var_name.clone(),
                                    VarValue::OscType(OscType::OscBlob(b.clone())),
                                ));

                                vars.insert(
                                    var_name.clone(),
                                    VarValue::OscType(OscType::OscBlob(b.clone())),
                                );
                            }
                            Some((_, Token::OscString(s))) => {
                                line_statements.push(Statement::Assignment(
                                    var_name.clone(),
                                    VarValue::OscType(OscType::OscString(s.clone())),
                                ));

                                vars.insert(
                                    var_name.clone(),
                                    VarValue::OscType(OscType::OscString(s.clone())),
                                );
                            }
                            _ => {
                                return Err(ParseError {
                                    line: line_n,
                                    token: i,
                                    kind: ParseErrorKind::UnexpectedToken,
                                })
                            }
                        },
                        Some((_, token)) => {
                            if let Some(VarValue::Addr(addr)) = vars.get(var_name) {
                                let mut args = vec![];

                                match token {
                                    Token::Addr(_) | Token::Eq => {
                                        return Err(ParseError {
                                            line: line_n,
                                            token: i,
                                            kind: ParseErrorKind::UnexpectedToken,
                                        })
                                    }
                                    Token::Var(var) => match vars.get(var) {
                                        None => {
                                            return Err(ParseError {
                                                line: line_n,
                                                token: i,
                                                kind: ParseErrorKind::UnknownVariable,
                                            })
                                        }
                                        Some(VarValue::Addr(s)) => {
                                            args.push(OscType::OscString(s.clone()));
                                        }
                                        Some(VarValue::OscType(t)) => {
                                            args.push(t.clone());
                                        }
                                    },
                                    Token::Int(i) => args.push(OscType::Int(*i)),
                                    Token::Float(f) => args.push(OscType::Float(*f)),
                                    Token::OscString(s) => args.push(OscType::OscString(s.clone())),
                                    Token::OscBlob(b) => args.push(OscType::OscBlob(b.clone())),
                                    Token::Comma => break,
                                };
                                while let Some((i, token)) = line.next() {
                                    match token {
                                        Token::Addr(_) | Token::Eq => {
                                            return Err(ParseError {
                                                line: line_n,
                                                token: i,
                                                kind: ParseErrorKind::UnexpectedToken,
                                            })
                                        }
                                        Token::Var(var) => match vars.get(var) {
                                            None => {
                                                return Err(ParseError {
                                                    line: line_n,
                                                    token: i,
                                                    kind: ParseErrorKind::UnknownVariable,
                                                })
                                            }
                                            Some(VarValue::Addr(s)) => {
                                                args.push(OscType::OscString(s.clone()));
                                            }
                                            Some(VarValue::OscType(t)) => {
                                                args.push(t.clone());
                                            }
                                        },
                                        Token::Int(i) => args.push(OscType::Int(*i)),
                                        Token::Float(f) => args.push(OscType::Float(*f)),
                                        Token::OscString(s) => {
                                            args.push(OscType::OscString(s.clone()))
                                        }
                                        Token::OscBlob(b) => args.push(OscType::OscBlob(b.clone())),
                                        Token::Comma => break,
                                    }
                                }
                                line_statements.push(Statement::Message(addr.clone(), args));
                            } else {
                                return Err(ParseError {
                                    line: line_n,
                                    token: i + 1,
                                    kind: ParseErrorKind::UnknownVariable,
                                });
                            }
                        }
                        None => {
                            // FIXME
                            return Err(ParseError {
                                line: line_n,
                                token: i + 1,
                                kind: ParseErrorKind::UnexpectedToken,
                            });
                        }
                    };
                }
                Token::Comma => (),
                Token::Eq => {
                    return Err(ParseError {
                        line: line_n,
                        token: i,
                        kind: ParseErrorKind::UnexpectedToken,
                    })
                }

                _ => {
                    return Err(ParseError {
                        line: line_n,
                        token: i,
                        kind: ParseErrorKind::UnexpectedToken,
                    })
                }
            }
        }
        statements.push(line_statements);
    }
    Ok((statements, vars))
}

#[derive(Debug)]
enum ParseErrorKind {
    UnexpectedToken,
    UnknownVariable,
}

#[derive(Debug)]
struct ParseError {
    line: usize,
    token: usize,
    kind: ParseErrorKind,
}

// LPM = 80 ; lines per minute
// /addr/addr/method 1.0 1 "hello world", /addr/method2
// /blob/thing #ffff_00ff

fn msg2real_msg(addr: String, args: &[OscType]) -> OscMessage {
    OscMessage {
        addr,
        args: args
            .iter()
            .map(|a| match a {
                OscType::Int(v) => rosc::OscType::Int(*v),
                OscType::Float(v) => rosc::OscType::Float(*v),
                OscType::OscString(v) => rosc::OscType::String(v.clone()),
                OscType::OscBlob(v) => rosc::OscType::Blob(v.clone()),
            })
            .collect(),
    }
}

// fn play(lines: &[Vec<Statement>])
