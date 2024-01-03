#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use] extern crate rocket;
#[macro_use] extern crate serde_derive;

use rocket::{
    http::Status,
    response::Redirect,
    outcome::try_outcome,
    fs::{NamedFile, FileServer},
    request::{FromRequest, Outcome},
    State,
    Request, Build, serde::json::Json,
};
use rocket_dyn_templates::Template;
use std::convert::From;
use std::thread::Builder;

mod language;

use language::{new_shared_language_cache, SharedLanguageCache, LangIcon, Language, Game as GameStrings};

#[derive(Serialize)]
struct LangTemplate {
    langs: Vec<LangIcon>,
    lang: Language,
}
#[rocket::async_trait]
impl<'r> FromRequest<'r> for LangTemplate {
    type Error = ();
    async fn from_request(req: &'r Request<'_>) -> Outcome<Self, Self::Error> {
        let langs = {
            language::langs(&mut try_outcome!(req.guard::<&State<SharedLanguageCache>>().await).inner().clone().lock().unwrap())
        };
        Outcome::Success(LangTemplate {
            langs,
            lang: try_outcome!(Language::from_request(req).await)
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
fn strings(code: String, slc: &State<SharedLanguageCache>) -> Option<Json<GameStrings>> {
    lang(code, slc).map(|l| Json(l.0.game))
}
#[get("/lang/<code>")]
fn lang(code: String, slc: &State<SharedLanguageCache>) -> Option<Json<Language>> {
    let dot = code.rfind('.')?;
    if &code[dot..] != ".json" {
        dbg!(code);
        return None;
    }
    slc.lock().unwrap().get(&code[..dot]).map(|l| Json(l))
}

#[get("/overview")]
fn overview(games: &State<WebSocketServer>) -> String {
    let games = games.inner().games.lock().unwrap();

    let mut s = String::new();

    for (code, g) in games.iter() {
        s.push_str(&format!("{}:\n{}\n", code, g.game));
    }

    s
}

#[get("/favicon.ico")]
async fn favicon() -> std::io::Result<NamedFile> {
    NamedFile::open("static/favicon.ico").await
}

#[derive(Debug, Serialize)]
struct Addr {
    ip: String,
}

#[rocket::async_trait]
impl<'r> FromRequest<'r> for Addr {
    type Error = ();
    async fn from_request(request: &'r Request<'_>) -> Outcome<Self, Self::Error> {
        match request.client_ip() {
            Some(ip) => Outcome::Success(Addr {ip: format!("{}", ip)}),
            None => Outcome::Error((Status::BadRequest, ()))
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
async fn not_found(req: &Request<'_>) -> Template {
    #[derive(Serialize)]
    struct NotFoundTemplate<'a> {
        path: &'a str,
        #[serde(flatten)]
        lang_template: LangTemplate
    }

    Template::render("error/404", &NotFoundTemplate{
        path: req.uri().path().as_str(),
        lang_template: LangTemplate::from_request(req).await.unwrap()
    })
}

#[inline]
fn rocket() -> rocket::Rocket<Build> {
    rocket::build()
        .mount("/static/", FileServer::from("static"))
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
        .register("/", catchers![not_found])
}

mod websocket_server;

use websocket_server::WebSocketServer;

#[rocket::launch]
fn rocket_() -> _ {
    let wss = WebSocketServer::new();

    let run_wss = wss.clone();

    Builder::new().name("websocket_server".to_owned()).spawn(move || {
        run_wss.run();
    }).unwrap();

    rocket()
        .manage(wss)
}
