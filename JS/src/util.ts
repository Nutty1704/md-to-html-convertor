import { commands } from "./commands";



// Function to apply all predefined commands using the regex from commands.ts
export function applyCommands(markdown: string): string {
    // For each command, apply the handler function to the markdown if the regex matches
    return commands.reduce((updatedMarkdown, {regex, handler}) => {
        // find the first match of the regex in the markdown
        const match = regex.exec(updatedMarkdown);
        // if a match is found, replace the match with the result of the handler function
        return match ? updatedMarkdown.replace(regex, handler(match)) : updatedMarkdown;
    }, markdown);
}

// Function to update the title tag in the HTML if a title is provided
export function updateTitle(title: string, html: string): string {
    if (title === "") {
        return html;
    }

    const titleRegex = /<title>.*<\/title>/;
    const titleTag = `<title>${title}</title>`;
    return html.replace(titleRegex, titleTag);
}