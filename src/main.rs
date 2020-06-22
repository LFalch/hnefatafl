#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use] extern crate rocket;
#[macro_use] extern crate serde_derive;

use rocket::{
    response::NamedFile,
    State,
    Request,
};
use rocket_contrib::templates::Template;
use rocket_contrib::{json::Json, serve::{StaticFiles}};
use std::collections::HashMap;
use std::convert::From;
use std::net::SocketAddr;
use std::thread;

#[derive(Serialize)]
struct SpelForm {
    code: Option<String>
}

#[derive(Serialize)]
struct Empty {

}

#[get("/")]
fn index() -> Template {
    Template::render("index", &Empty{})
}
#[get("/spel?<code>")]
fn spel(code: Option<String>) -> Template {
    Template::render("spel", &SpelForm{code})
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

#[get("/overview")]
fn overview(games: State<WebSocketServer>) -> String {
    let games = games.inner().games.lock().unwrap();

    let mut s = String::new();

    for (code, g) in games.iter() {
        s.push_str(&format!("{}:\n{}\n", code, g.game));
    }

    s
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
                overview,
            ],
        )
        .attach(Template::fairing())
        .register(catchers![not_found])
}

mod websocket_server;

use websocket_server::WebSocketServer;

fn main() {
    let wss = WebSocketServer::new();

    let run_wss = wss.clone();

    thread::spawn(move || {
        run_wss.run();
    });

    rocket().manage(wss).launch();
}
