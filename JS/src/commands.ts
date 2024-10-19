type Executor = (match: RegExpMatchArray) => string;

interface Command {
    regex: RegExp;
    handler: Executor;
}


// Command 1: Table generation command
const tableCommand: Command = {
    regex: /TAB(\d+)x(\d+)\./, // Matches TABNxM
    handler: (match: RegExpMatchArray): string => {
        const rows = parseInt(match[1], 10);
        const columns = parseInt(match[2], 10);
        return generateTable(rows, columns);
    }
};


const imageCommand: Command = {
    regex: /IMG\|([^|]+)\|([^|]+)\|([^|]+)\|/, // Matches IMG|alt|link|caption|
    handler: (match: RegExpMatchArray): string => {
        const alt = match[1];
        const link = match[2];
        const caption = match[3];
        return `![${alt}](${link} "${caption}")`;
    }
};

export const commands: Command[] = [tableCommand, imageCommand];


// Helper function to generate a table
function generateTable(rows: number, columns: number): string {
    const headerRow = `| ${Array(columns).fill("Heading").map((h, i) => h + ' ' + (i + 1)).join(" | ")} |`;
    const separatorRow = `| ${Array(columns).fill("---------").join(" | ")} |`;
    const contentRows = Array(rows).fill(`| ${Array(columns).fill("Item").map((i, j) => i + ' ' + (j + 1)).join(" | ")} |`);
    return [headerRow, separatorRow, ...contentRows].join("\n");
}


