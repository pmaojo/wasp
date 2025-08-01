import type { NextApiRequest, NextApiResponse } from "next";
import { getRuns } from "../../services/cliService";

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse,
) {
  if (req.method !== "GET") {
    res.status(405).end();
    return;
  }
  const runs = await getRuns();
  res.status(200).json(runs);
}
