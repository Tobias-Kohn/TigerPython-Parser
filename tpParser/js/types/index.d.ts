declare module "tigerpython-parser" {
    class ErrorInfo {
        line: number;
        offset: number;
        msg: string;
        code: string;
    }
    export namespace TPyParser {
        let evalMode: boolean;
        let newDivision: boolean;
        let pythonVersion: number;
        let rejectDeadCode: boolean;
        let repreatStatement: boolean;
        let sagePower: boolean;
        let translateUnicodePunctuation: boolean;
        let warningAsErrors: boolean;

        function getLanguage(): string;
        function setLanguage(language: string): void;
        function setErrorMessage(code: string, msg: string): void;
        function checkSyntax(source: string): ErrorInfo;
        function findAllErrors(source: string): ErrorInfo[];
        function parse(source: string): any;
    }
}
