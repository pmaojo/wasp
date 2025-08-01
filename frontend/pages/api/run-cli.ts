import type { NextApiRequest, NextApiResponse } from "next";
import { executeCommand, prepareCommand } from "../../services/cliService";

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse,
) {
  if (req.method !== "POST") {
    res.status(405).end();
    return;
  }
  const { command, args = [] } = req.body || {};
  const cmd = prepareCommand(command, Array.isArray(args) ? args : []);
  const run = await executeCommand(cmd);
  res.status(200).json(run);
}
