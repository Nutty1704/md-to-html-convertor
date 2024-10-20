type Executor = (match: RegExpMatchArray) => string;

// Command interface, with a regex to match and a handler to execute
interface Command {
    regex: RegExp;
    handler: Executor;
}


// Table generation command
const tableCommand: Command = {
    regex: /TAB(\d+)x(\d+)\./, // Matches TABNxM
    handler: (match: RegExpMatchArray): string => {
        const rows = parseInt(match[1], 10);
        const columns = parseInt(match[2], 10);
        return generateTable(rows, columns);
    }
};

// Image generation command
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


// Helper Functions

// Helper function to generate a table
function generateTable(rows: number, columns: number): string {
    const headerRow = `| ${Array(columns)
        .fill("Heading")
        .map((h, i) => h + ' ' + (i + 1))  // Add the column number to the heading
        .join(" | ")} |`;

    const separatorRow = `| ${Array(columns)
        .fill("---------")
        .join(" | ")} |`;

    const contentRows = Array(rows - 1)
        .fill(`| ${Array(columns)
        .fill("Item")
        .map((i, j) => i + ' ' + (j + 1))  // Add the column number to the item
        .join(" | ")} |`);

    return [headerRow, separatorRow, ...contentRows].join("\n");
}


