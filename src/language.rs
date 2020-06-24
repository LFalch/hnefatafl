use std::fs::File;
use std::path::Path;
use serde_json::from_reader;

use rocket::Request;
use rocket::http::Cookie;
use rocket::request::{FromRequest, Outcome};

use rocket_accept_language::AcceptLanguage;

#[derive(Debug, Clone, Serialize)]
pub struct LangIcon {
    code: String,
    name: String
}

pub fn langs() -> Vec<LangIcon> {
    Path::new("languages/").read_dir().unwrap()
        .filter_map(|entry| {
            let path = entry.ok()?.path();
            let file = File::open(&path).ok()?;
            let lang: Language = from_reader(file).ok()?;
            Some(LangIcon {
                code: path.file_stem()?.to_os_string().into_string().ok()?,
                name: lang.display_name,
            })
        })
        .collect()
}

pub fn get_language(code: &str) -> Option<Language> {
    let languages = Path::new("languages/");

    let mut path = languages.join(code);
    path.set_extension("json");
    if path.parent() != Some(languages) {
        return None;
    }
    let file = File::open(path).ok()?;
    from_reader(file).ok()
}

impl FromRequest<'_, '_> for Language {
    type Error = ();
    fn from_request(req: &Request) -> Outcome<Self, Self::Error> {
        let mut cookies = req.cookies();
        let code = if let Some(cookie) = cookies.get("lang") {
            cookie.value()
        } else {
            for locale in AcceptLanguage::from_request(req).unwrap().accept_language {
                let code = locale.get_language();

                if let Some(lang) = get_language(code) {
                    cookies.add(Cookie::new("lang", code.to_owned()));
                    return Outcome::Success(lang);
                }
            }
            ""
        };
        Outcome::Success(
            get_language(code).unwrap_or_else(|| get_language("no").unwrap())
        )
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Language {
    pub lang_code: String,
    pub display_name: String,
    pub index_title: String,
    pub welcome: String,
    pub new_game: String,
    pub or_join: String,
    pub game_code: String,
    pub join: String,

    pub not_found_title: String,
    pub the_page_was_not_found: String,

    pub game_title: String,
    pub special_thanks: String,
    pub write_to_opponent_here: String,

    pub rules: Rules,
    pub game: Game,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Rules {
    rules: String,
    intro: String,
    placing_pieces_header: String,
    placing_pieces: String,
    moving_pieces_header: String,
    moving_pieces: String,
    taking_pieces_header: String,
    taking_pieces: String,
    hirdmann_killed_header: String,
    hirdmann_killed: String,
    aatakar_killed_header: String,
    aatakar_killed: String,
    who_wins_header: String,
    who_wins: String,
    king_killed_header: String,
    king_killed: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Game {
    close_error: String,
    end: TrueFalse,
    code: String,
    host_success: String,
    join_fail: String,
    join_success: String,
    your_turn: String,
    opponents_turn: String,
    game_start: String,
    game_start2: TrueFalse,
    game_win: String,
    game_lose: String,
    team_aatak: String,
    team_hirdi: String,
    unknown: String
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct TrueFalse {
    r#true: String,
    r#false: String,
}