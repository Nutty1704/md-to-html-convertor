export type { State, SaveData };

type State = Readonly<{
    markdown: string;
    HTML: string;
    renderHTML: boolean;
    save: boolean;
    saveData: SaveData | null;
}>;

type SaveData = Readonly<{
    success: boolean;
    message: string;
}>;
