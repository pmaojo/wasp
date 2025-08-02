import type { NextApiRequest, NextApiResponse } from "next";
import { getRun } from "../../../services/cliService";

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse,
) {
  if (req.method !== "GET") {
    res.status(405).end();
    return;
  }
  const { id } = req.query;
  const run = typeof id === "string" ? await getRun(id) : null;
  if (!run) {
    res.status(404).end();
    return;
  }
  res.status(200).json(run);
}
