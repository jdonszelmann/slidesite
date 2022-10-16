use maud::{html, DOCTYPE, Render, PreEscaped};
use crate::eval::value::{SlideShow, Slide};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum EmitError {

}

pub fn emit(ast: SlideShow) -> Result<String, EmitError> {
    let title = ast.title.unwrap_or_else(|| {
        eprintln!("no title set. using placeholder title");
        "Placeholder Title".to_string()
    });
    let title = title.trim();

    let res: PreEscaped<String> = html! {
        (DOCTYPE)

        html lang="en" {
        head {
            meta charset="UTF-8";
            title {(title)};
            style {
                (PreEscaped(include_str!("base_style.css")))
            }
            script {
                (PreEscaped(include_str!("main.js")))
            }
        }
        body {
            div id="homepage" {
                (emit_sidebar(&ast.slides)?);
                (PreEscaped(include_str!("homepage.html").replace("$TITLE", title)));

                div id="slides" {
                    @for (index, slide) in ast.slides.into_iter().enumerate() {
                        (emit_slide(index, slide)?)
                    }
                }
            }
        }
        }
    };

    Ok(res.into_string())
}

pub fn emit_sidebar(slides: &[Slide]) -> Result<impl Render, EmitError> {
    Ok(html! {
        div id="sidebar" {
            div class="sidebaritem" title="back home" onclick="home_mode();" {
                span class="index" {(format!("  🏠. "))}
                span{"Back Home"}
            }
            @for (index, slide) in slides.iter().enumerate() {
                div class="sidebaritem" id={"sidebar-" (slide.identifier)} title={"Go to slide " (slide.title)} onclick={"goto_slide("(PreEscaped(format!("'{}'", slide.identifier)))")"} {
                    span class="index" {(format!("{index: >3}. "))}
                    span{(slide.title)}
                }
            }
        }
    })
}

pub fn emit_slide(num: usize, slide: Slide) -> Result<impl Render, EmitError> {
    Ok(html! {
        div class="slide" id=(slide.identifier) name=(slide.title) data-slidenum=(&(num + 1)) {
            h1 class="slide-title" {
                (slide.title)
            }
        }
    })
}
