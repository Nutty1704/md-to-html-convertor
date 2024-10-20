import { commands } from "./commands";



// Function to apply all predefined commands using the regex from commands.ts
export function applyCommands(markdown: string): string {
    return commands.reduce((updatedMarkdown, {regex, handler}) => {
        const match = regex.exec(updatedMarkdown);
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