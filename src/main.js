
const slides = new Map();
let mode = "home";
let homepage = null;
let slides_elem = null;
let active_id = null;

document.oninput = () => {

}

class Slide {
    name
    identifier

    constructor(name, identifier) {
        this.name = name;
        this.identifier = identifier;
    }
}
const params = new Proxy(new URLSearchParams(window.location.search), {
    get: (searchParams, prop) => searchParams.get(prop),
});

document.onreadystatechange = () => {
    for (const i of Array.from(document.getElementsByClassName("slide"))) {
        let slidenum = parseInt(i.dataset.slidenum);

        slides.set(slidenum, new Slide(
            i.getAttribute("name"),
            i.getAttribute("id"),
        ))
    }

    homepage = document.getElementById("main-homepage");
    slides_elem = document.getElementById("slides");

    slides_elem.addEventListener("scroll", find_active_slide);

    if (typeof params.slide !== "undefined") {
        const slidenum = parseInt(params.slide);
        if (!isNaN(slidenum)) {
            goto_slide(slides.get(slidenum).identifier)
        } else {
            if (!goto_slide(params.slide)){
                for (const i of slides.values()) {
                    if (i.name.toLowerCase().includes(params.slide.toLowerCase())) {
                        goto_slide(i.identifier);
                        break;
                    }
                }
            }
        }
    }
};

function remove_js_styles() {
    let styled = document.querySelectorAll("*[style]");
    for(let i=0; i<styled.length;i++) {
        styled[i].removeAttribute("style");
    }
}

function find_active_slide() {
    let passed = new Set();

    [].slice.call(slides_elem.children).forEach(function (ele, index) {
        const is_passed = ele.getBoundingClientRect().top < slides_elem.getBoundingClientRect().top - 10;

        if (is_passed) {
            passed.add(ele.id)
        }
    });

    let lowest = Infinity;
    let lowest_id = null;

    for (const [number, value] of slides) {
        if (!passed.has(value.identifier) && number < lowest) {
            lowest = number;
            lowest_id = value.identifier;
        }
    }

    if (lowest_id == null) {
        active_id = slides.get(slides.size).identifier;
    } else {
        active_id = lowest_id;
    }

    highlight_active_sidebar();
}

function highlight_active_sidebar() {
    for (const i of document.getElementsByClassName("sidebaritem")) {
        i.classList.remove("active")
    }

    if (mode === "slides") {
        const id = "sidebar-" + active_id;
        document.getElementById(id).classList.add("active");
    }
}

function home_mode() {
    mode = "home";
    remove_js_styles();

    highlight_active_sidebar();
}

function slides_mode() {
    if (mode !== "home") {
        home_mode();
    }

    mode = "slides";
    homepage.style.display = "none";
    slides_elem.style.display = "block";

    find_active_slide();
}

function scroll_through_slides() {
    slides_mode();
    goto_slide(slides.get(1).identifier);
}

function goto_slide(identifier) {
    const slide = document.getElementById(identifier);

    if (typeof slide === "undefined" || slide == null) {
        return false;
    }

    slides_mode();
    slide.scrollIntoView({ behavior: 'smooth', block: 'center' })

    return true;
}

function slideshow() {

}