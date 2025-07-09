export default {
  transform: { "^.+\\.tsx?$": "ts-jest" },
  testEnvironment: "jest-environment-jsdom",
  testRegex: "/test/.*\\.test\\.tsx?$",
  moduleFileExtensions: ["ts", "tsx", "js", "jsx"],
};
