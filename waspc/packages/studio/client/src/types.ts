export type StudioEntity = {
  name: string;
};

export type StudioPage = {
  name: string;
  authRequired?: boolean | null;
  operations: string[];
};

export type PageRef = {
  name: string;
};

export type StudioRoute = {
  name: string;
  path: string;
  toPage: PageRef;
};

export type HttpRoute = {
  method: string;
  path: string;
};

export type StudioApi = {
  name: string;
  httpRoute: HttpRoute;
  auth?: boolean | null;
  entities: StudioEntity[];
};

export type StudioJob = {
  name: string;
  schedule?: string | null;
  entities: StudioEntity[];
};

export type OperationType = "query" | "action";

export type StudioOperation = {
  type: OperationType;
  name: string;
  entities: StudioEntity[];
  auth?: boolean | null;
};

export type StudioCrud = {
  name: string;
  operations: string[];
  entities: StudioEntity[];
};

export type StudioAppAuth = {
  userEntity: StudioEntity;
  methods: string[];
};

export type StudioDb = {
  system: string;
};

export type StudioApp = {
  name: string;
  auth: StudioAppAuth;
  db: StudioDb;
};

export type StudioData = {
  pages: StudioPage[];
  routes: StudioRoute[];
  apis: StudioApi[];
  jobs: StudioJob[];
  operations: StudioOperation[];
  cruds: StudioCrud[];
  entities: StudioEntity[];
  app: StudioApp;
};
