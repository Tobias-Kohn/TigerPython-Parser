# Releasing

## Prerequisites

- [sbt](https://www.scala-sbt.org/) matching the version pinned in [project/build.properties](project/build.properties)
  (currently 1.9.9), with a JDK on your `PATH` (Java 17+).

- An npm account that is a member of the `tigerpython` organisation with publish rights to `@tigerpython/tpparser`,
  logged in locally via `npm login`.

## Steps

The package is published to the npm registry under the `tigerpython` organisation as
[`@tigerpython/tpparser`](https://www.npmjs.com/package/@tigerpython/tpparser). To cut a new release:

1. Bump `releaseVersion` in [build.sbt](build.sbt) and merge that change to `master`.
2. From the project root, run `sbt makeRelease`. This regenerates the `/release` folder as well as `package.json`
   and `package-lock.json` from the templates in `build.sbt`, so those files always stay in sync with the version
   number.
3. Run `npm publish`.

This is a manual, deliberate process rather than something that runs automatically on every merge to `master`: npm
access tokens now require an expiry date, and since this project only releases occasionally, an unattended CI
pipeline risks silently breaking the day the token expires with nobody noticing until the next release. A
[GitHub Actions workflow](.github/workflows/publish-npm.yml) that triggers on a published GitHub Release is included
as an optional alternative — using it requires adding an `NPM_TOKEN` secret (a granular access token scoped to
`@tigerpython/tpparser`) to the repository, and remembering to rotate it before it expires.
