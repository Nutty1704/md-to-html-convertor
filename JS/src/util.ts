import { commands } from "./commands";



// Function to apply all predefined commands using the regex from commands.ts
export function applyCommands(markdown: string): string {
    return commands.reduce((updatedMarkdown, {regex, handler}) => {
        const match = regex.exec(updatedMarkdown);
        return match ? updatedMarkdown.replace(regex, handler(match)) : updatedMarkdown;
    }, markdown);
}