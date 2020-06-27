#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use] extern crate rocket;
#[macro_use] extern crate serde_derive;

use rocket::{
    http::Status,
    response::{NamedFile, Redirect},
    request::{FromRequest, Outcome},
    State,
    Request,
};
use rocket_contrib::templates::Template;
use rocket_contrib::{json::Json, serve::{StaticFiles}};
use std::convert::From;
use std::thread::Builder;

mod language;

use language::{new_shared_language_cache, SharedLanguageCache, LangIcon, Language, Game as GameStrings};

#[derive(Serialize)]
struct LangTemplate {
    langs: Vec<LangIcon>,
    lang: Language,
}
impl FromRequest<'_, '_> for LangTemplate {
    type Error = ();
    fn from_request(req: &Request) -> Outcome<Self, Self::Error> {
        let langs = {
            language::langs(&mut req.guard::<State<SharedLanguageCache>>()?.inner().clone().lock().unwrap())
        };
        Outcome::Success(LangTemplate {
            langs,
            lang: Language::from_request(req)?
        })
    }
}

#[get("/index.html")]
#[inline(always)]
fn index_html() -> Redirect {
    Redirect::to(uri!(index))
}
#[get("/index.php")]
#[inline(always)]
fn index_php() -> Redirect {
    Redirect::permanent(uri!(index))
}

#[get("/")]
fn index(lt: LangTemplate) -> Template {
    Template::render("index", &lt)
}
#[get("/spel?<code>")]
#[allow(unused_variables)]
fn spel(lt: LangTemplate, code: Option<String>) -> Template {
    Template::render("spel", &lt)
}
#[get("/strings/<code>")]
fn strings(code: String, slc: State<SharedLanguageCache>) -> Option<Json<GameStrings>> {
    lang(code, slc).map(|l| Json(l.0.game))
}
#[get("/lang/<code>")]
fn lang(code: String, slc: State<SharedLanguageCache>) -> Option<Json<Language>> {
    let dot = code.rfind('.')?;
    if &code[dot..] != ".json" {
        dbg!(code);
        return None;
    }
    slc.lock().unwrap().get(&code[..dot]).map(|l| Json(l))
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

#[derive(Debug, Serialize)]
struct Addr {
    ip: String,
}

impl FromRequest<'_, '_> for Addr {
    type Error = ();
    fn from_request(request: &Request) -> Outcome<Self, Self::Error> {
        match request.client_ip() {
            Some(ip) => Outcome::Success(Addr {ip: format!("{}", ip)}),
            None => Outcome::Failure((Status::BadRequest, ()))
        }
    }
}

#[get("/ip")]
fn ip(ip: Addr) -> String {
    ip.ip
}
#[get("/ip.json")]
fn ip_json(ip: Addr) -> Json<Addr> {
    Json(ip)
}
#[get("/robots.txt")]
fn robots() -> &'static str {
r"User-agent: *
Disallow: /
"
}

#[catch(404)]
fn not_found(req: &Request) -> Template {
    #[derive(Serialize)]
    struct NotFoundTemplate<'a> {
        path: &'a str,
        #[serde(flatten)]
        lang_template: LangTemplate
    }

    Template::render("error/404", &NotFoundTemplate{
        path: req.uri().path(),
        lang_template: LangTemplate::from_request(req).unwrap()
    })
}

#[inline]
fn rocket() -> rocket::Rocket {
    rocket::ignite()
        .mount("/", StaticFiles::from("static"))
        .mount(
            "/",
            routes![
                spel,
                index,
                index_html,
                index_php,
                favicon,
                overview,
                strings,
                lang,
                ip,
                ip_json,
                robots,
            ],
        )
        .attach(Template::fairing())
        .manage(new_shared_language_cache())
        .register(catchers![not_found])
}

mod websocket_server;

use websocket_server::WebSocketServer;

fn main() {
    let wss = WebSocketServer::new();

    let run_wss = wss.clone();

    Builder::new().name("websocket_server".to_owned()).spawn(move || {
        run_wss.run();
    }).unwrap();

    rocket().manage(wss).launch();
}
