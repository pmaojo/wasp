module.exports = {
  preset: "ts-jest",
  testEnvironment: "jsdom",
  globals: { "ts-jest": { tsconfig: "tsconfig.jest.json" } },
  transform: {
    "^.+\\.(ts|tsx)$": "ts-jest",
  },
  moduleFileExtensions: ["ts", "tsx", "js"],
  setupFilesAfterEnv: ["./setupTests.ts"],
};
