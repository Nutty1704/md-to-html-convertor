import { fromEvent, merge } from "rxjs";
import { map, mergeScan, first } from "rxjs/operators";
import { ajax } from "rxjs/ajax";
import { type Observable } from "rxjs";
import { State } from "./types";
import { applyCommands, updateTitle } from "./util";

import hljs from "highlight.js/lib/core";

import javascript from "highlight.js/lib/languages/javascript";
import python from "highlight.js/lib/languages/python";
import haskell from "highlight.js/lib/languages/haskell";

// Load the languages from the unit for syntax highlighting!
hljs.registerLanguage("javascript", javascript);
hljs.registerLanguage("python", python);
hljs.registerLanguage("haskell", haskell);

const markdownInput = document.getElementById(
    "markdown-input",
) as HTMLTextAreaElement;
const checkbox = document.querySelector('input[name="checkbox"]')!;
const saveHTMLButton = document.getElementById("save-html") as HTMLButtonElement;
const titleInput = document.getElementById("html-title-input") as HTMLInputElement;

type Action = (_: State) => State;

const resetState: Action = (s) => {
    return { ...s, saveData: null };
};

const compose =
    <T, U>(g: (_: T) => U) =>
    <V>(f: (_: U) => V) =>
    (t: T): V =>
        f(g(t));

// Create an Observable for keyboard input events
const input$: Observable<Action> = fromEvent<KeyboardEvent>(
    markdownInput,
    "input",
).pipe(
    map((event) => (event.target as HTMLInputElement).value),
    map((value) => applyCommands(value)),  // Apply applicable shortcuts/commands to the markdown
    map((value) => (s) => ({ ...s, markdown: value })),
);

const checkboxStream$: Observable<Action> = fromEvent(checkbox, "change").pipe(
    map((event) => (event.target as HTMLInputElement).checked),
    map((value) => (s) => ({ ...s, renderHTML: value })),
);

// Observable for save button click
const saveHTML$: Observable<Action> = fromEvent(saveHTMLButton, "click").pipe(
    map(() => (s) => ({...s, save: true}))
);

const titleInput$: Observable<Action> = fromEvent(titleInput, "input").pipe(
    map((event) => (event.target as HTMLInputElement).value),
    map((value) => (s) => ({ ...s, title: value })),
);

function getHTML(s: State): Observable<State> {
    // Get the HTML as a stream
    return ajax<{ html: string }>({
        url: "/api/convertMD",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: s.markdown,
    }).pipe(
        map((response) => response.response), // Extracting the response data
        map((data) => {
            return {
                ...s,
                HTML: data.html,
            };
        }),
        first(),
    );
}

// Function to save the HTML to the server
function saveHTML(s: State): Observable<State> {

    return ajax<{ success: boolean, message: string }>({
        url: "/api/saveHTML",
        method: "POST",
        headers: {
            "Content-Type": "text/html",
        },
        body: updateTitle(s.title, s.HTML),  // Send the HTML with the updated title
    }).pipe(
        map((response) => {
            // Return updated state with the saveData
            return {
                ...s,
                save: false,  // reset the save field
                saveData: {
                    message: response.response.message,
                    success: response.response.success,
                }
            };
        }),
        first(),
    );
}


const initialState: State = {
    markdown: "",
    HTML: "",
    renderHTML: true,
    save: false,
    saveData: null,
    title: ""
};

function main() {
    // Subscribe to the input Observable to listen for changes
    const subscription = merge(input$, checkboxStream$, saveHTML$, titleInput$)
        .pipe(
            map((reducer: Action) => {
                // Reset Some variables in the state in every tick
                const newReducer = compose(reducer)(resetState);
                return newReducer;
            }),
            mergeScan((acc: State, reducer: Action) => {
                const newState = reducer(acc);
                // If the save flag is set, save the HTML
                if (newState.save) {
                    return saveHTML(newState);
                }
                // getHTML returns an observable of length one
                // so we `scan` and merge the result of getHTML in to our stream
                return getHTML(newState);
            }, initialState),
        )
        .subscribe((value) => {
            const htmlOutput = document.getElementById("html-output");
            if (htmlOutput) {
                htmlOutput.innerHTML = "";
                htmlOutput.textContent = "";
                if (value.renderHTML) {
                    const highlight =
                        '<link rel="stylesheet" href="https://unpkg.com/@highlightjs/cdn-assets@11.3.1/styles/default.min.css" />';
                    htmlOutput.innerHTML = highlight + updateTitle(value.title, value.HTML);
                    // Magic code to add code highlighting
                    const blocks = htmlOutput.querySelectorAll("pre code");
                    blocks.forEach((block) =>
                        hljs.highlightElement(block as HTMLElement),
                    );
                } else {
                    htmlOutput.textContent = updateTitle(value.title, value.HTML);
                }
            }

            // Show the save message if it exists
            if (value.saveData) {
                alert(value.saveData.message);
            }

            // Update the markdown input with the new value after applying shortcuts
            markdownInput.value = value.markdown;
        });
}
if (typeof window !== "undefined") {
    window.onload = function () {
        main();
    };
}
