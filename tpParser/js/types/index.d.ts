declare module "tigerpython-parser" {
    interface ErrorInfo {
        line: number;
        offset: number;
        msg: string;
        code: string;
    }
    interface Completion {
        acResult: string; // the name of the item (ac=autocomplete)
        documentation: string | null;
        type: string | null;
        params: string[] | null;
        signature: Signature | null;
    }
    interface SignatureArg {
      name: string;
      defaultValue: string | null;
      argType: string | null;
    }

    interface SignatureVarArg {
      name: string;
      argType: string | null;
    }

    interface Signature {
      positionalOnlyArgs: SignatureArg[];      // before /
      positionalOrKeywordArgs: SignatureArg[]; // after / before *
      varArgs: SignatureVarArg | null;         // *args
      keywordOnlyArgs: SignatureArg[];         // after *
      varKwargs: SignatureVarArg | null;       // **kwargs
      firstParamIsSelfOrCls: boolean;
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
        function defineModule(moduleName: string, moduleBody: string, sourceFormat?: "pyi" | "tj" | "legacy" | null) : void;
    }
}
