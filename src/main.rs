#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use] extern crate rocket;
#[macro_use] extern crate serde_derive;

use rocket::{
    response::{NamedFile, Redirect},
    request::{FromRequest, Outcome},
    State,
    Request,
};
use rocket_contrib::templates::Template;
use rocket_contrib::{json::Json, serve::{StaticFiles}};
use std::convert::From;
use std::thread;

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
fn index_html(lt: LangTemplate) -> Template {
    index(lt)
}
#[get("/index.php")]
#[inline(always)]
fn index_php() -> Redirect {
    Redirect::permanent("/")
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
fn lang(code: String, slc: State<SharedLanguageCache>) -> Option<Json<GameStrings>> {
    let dot = code.rfind('.')?;
    if &code[dot..] != ".json" {
        dbg!(code);
        return None;
    }
    slc.lock().unwrap().get(&code[..dot]).map(|l| Json(l.game))
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
        .mount("/static/", StaticFiles::from("static"))
        .mount(
            "/",
            routes![
                spel,
                index,
                index_html,
                index_php,
                favicon,
                overview,
                lang,
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

    thread::spawn(move || {
        run_wss.run();
    });

    rocket().manage(wss).launch();
}
