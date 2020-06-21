#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use] extern crate rocket;
#[macro_use] extern crate serde_derive;

use rocket::{
    response::NamedFile,
    Request,
};
use rocket_contrib::templates::Template;
use rocket_contrib::{json::Json, serve::{StaticFiles}};
use std::collections::HashMap;
use std::convert::From;
use std::net::SocketAddr;

#[derive(Serialize)]
struct TemplateContext {
    name: String,
}

#[derive(Serialize)]
struct Empty {

}

#[get("/")]
fn index() -> Template {
    let context = TemplateContext {
        name: "index".to_string(),
    };
    Template::render("index", &context)
}
#[get("/spel")]
fn spel() -> Template {
    Template::render("spel", &Empty{})
}

#[get("/ip")]
fn ip(addr: SocketAddr) -> String {
    format!("{}\n", addr.ip())
}

#[derive(Serialize)]
struct Ip {
    ip: String,
}

#[get("/ip.json")]
fn ip_json(addr: SocketAddr) -> Json<Ip> {
    Json(Ip {
        ip: format!("{}", addr.ip())
    })
}

#[get("/favicon.ico")]
fn favicon() -> std::io::Result<NamedFile> {
    NamedFile::open("static/favicon.ico")
}

#[catch(404)]
fn not_found(req: &Request) -> Template {
    let mut map = HashMap::new();
    map.insert("path", req.uri().path());
    Template::render("error/404", &map)
}

#[inline]
fn rocket() -> rocket::Rocket {
    rocket::ignite()
        // Have Rocket manage the database pool.
        .mount("/static/", StaticFiles::from("static"))
        .mount(
            "/",
            routes![
                spel,
                index,
                ip,
                ip_json,
                favicon,
            ],
        )
        .attach(Template::fairing())
        .register(catchers![not_found])
}

use std::thread;

use websocket::OwnedMessage;
use websocket::sync::Server;

use std::sync::{Arc, Mutex};

fn main() {
    thread::spawn(|| {
        let server = Server::bind("127.0.0.1:2794").unwrap();
        const PROTOCOL: &str = "hnefatafl";

        let messages = Arc::new(Mutex::new(Vec::<String>::new()));

        for request in server.filter_map(Result::ok) {
            let messages = messages.clone();
            // Spawn a new thread for each connection.
            thread::spawn(move || {
                let mut num_messages = messages.lock().unwrap().len();
                if !request.protocols().contains(&PROTOCOL.to_owned()) {
                    request.reject().unwrap();
                    return;
                }

                let client = request.use_protocol(PROTOCOL).accept().unwrap();

                let ip = client.peer_addr().unwrap();

                println!("Connection from {}", ip);

                // let message = OwnedMessage::Text("Hello".to_string());
                // client.send_message(&message).unwrap();

                let (mut receiver, mut sender) = client.split().unwrap();

                for message in receiver.incoming_messages() {
                    let message = message.unwrap();

                    {
                        let msgs = messages.lock().unwrap();
                        
                        while msgs.len() > num_messages {
                            let msg_to_send = OwnedMessage::Text(msgs[num_messages].clone());
                            sender.send_message(&msg_to_send).unwrap();
                            num_messages += 1;
                        }
                    }

                    match message {
                        OwnedMessage::Close(_) => {
                            let message = OwnedMessage::Close(None);
                            sender.send_message(&message).unwrap();
                            println!("Client {} disconnected", ip);
                            return;
                        }
                        OwnedMessage::Ping(ping) => {
                            let message = OwnedMessage::Pong(ping);
                            sender.send_message(&message).unwrap();
                        }
                        OwnedMessage::Text(text) => {
                            eprintln!("Got {:?}", text);
                            let mut msgs = messages.lock().unwrap();
                            
                            msgs.push(text);
                        }
                        _ => sender.send_message(&message).unwrap(),
                    }
                }
            });
        }
    });

    rocket().launch();
}
