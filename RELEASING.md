# Releasing

The package is published to the npm registry under the `tigerpython` organisation as
[`@tigerpython/tpparser`](https://www.npmjs.com/package/@tigerpython/tpparser). Releasing is automatic: bump
`releaseVersion` in [build.sbt](build.sbt) and merge that change to `master`.

That push triggers the [publish-npm.yml](.github/workflows/publish-npm.yml) GitHub Actions workflow, which:

1. Checks whether the version in `build.sbt` is already published to npm. If so, it stops here — this makes it
   safe for the workflow to run on every push to `master`, since only an actual version bump does anything.
2. Runs the test suite (`sbt test`).
3. Builds the release artifacts (`sbt makeRelease`), which regenerates the `/release` folder as well as
   `package.json` and `package-lock.json` from the templates in `build.sbt`.
4. Publishes to npm via [trusted publishing](https://docs.npmjs.com/trusted-publishers) (OIDC) — no npm token or
   secret involved.
5. Creates a GitHub Release for the new version.

No manual `npm publish` or npm login is needed for a normal release.
