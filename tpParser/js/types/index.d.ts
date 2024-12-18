declare module "tigerpython-parser" {
    class ErrorInfo {
        line: number;
        offset: number;
        msg: string;
        code: string;
    }
    class Completion {
        acResult: string;
        documentation: string;
        type: string;
        params: string[] | null;
    }
    export namespace TPyParser {
        let evalMode: boolean;
        let newDivision: boolean;
        let pythonVersion: number;
        let rejectDeadCode: boolean;
        let repeatStatement: boolean;
        let sagePower: boolean;
        let translateUnicodePunctuation: boolean;
        let warningAsErrors: boolean;

        function getLanguage(): string;
        function getLanguages(): string[];
        function setLanguage(language: string): void;
        function setErrorMessage(code: string, msg: string): void;
        function checkSyntax(source: string): ErrorInfo;
        function findAllErrors(source: string): ErrorInfo[];
        function parse(source: string): any;
        function autoComplete(source: string, pos: number, filter: boolean) : string[];
        function autoCompleteExt(source: string, pos: number) : Completion[] | null;
        function defineModule(moduleName: string, moduleBody: string) : void;
    }
}
