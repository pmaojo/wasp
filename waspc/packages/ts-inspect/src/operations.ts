import * as fs from "fs/promises";
import JSON5 from "json5";
import * as path from "path";
import ts from "typescript";
import { z } from "zod";

export const OperationsRequest = z.object({
  tsconfig: z.string().optional(),
  filepaths: z.array(z.string()),
});

export const OperationsRequests = z.array(OperationsRequest);

export type OperationsRequest = z.infer<typeof OperationsRequest>;

export async function getOperationsOfFiles(
  request: OperationsRequest,
): Promise<{ [file: string]: string[] }> {
  let compilerOptions: ts.CompilerOptions = {};

  if (request.tsconfig) {
    compilerOptions = await loadCompilerOptionsFromTsConfig(request.tsconfig);
  }

  const operationsMap: { [file: string]: string[] } = {};

  const program = ts.createProgram(request.filepaths, compilerOptions);

  for (const filename of request.filepaths) {
    const source = program.getSourceFile(filename);
    if (!source) {
      operationsMap[filename] = [];
      continue;
    }
    const ops: string[] = [];
    function visit(node: ts.Node) {
      if (ts.isCallExpression(node)) {
        const expr = node.expression;
        if (ts.isIdentifier(expr) && (expr.text === "useQuery" || expr.text === "useAction")) {
          const arg = node.arguments[0];
          if (arg && ts.isIdentifier(arg)) {
            ops.push(arg.text);
          }
        }
      }
      ts.forEachChild(node, visit);
    }
    visit(source);
    operationsMap[filename] = ops;
  }

  return operationsMap;
}

async function loadCompilerOptionsFromTsConfig(
  tsconfig: string,
): Promise<ts.CompilerOptions> {
  const configJson = JSON5.parse(await fs.readFile(tsconfig, "utf8"));
  const basePath = path.dirname(tsconfig);

  const { options, errors } = ts.convertCompilerOptionsFromJson(
    configJson.compilerOptions,
    basePath,
    tsconfig,
  );
  if (errors && errors.length) {
    throw errors;
  }
  return options;
}

