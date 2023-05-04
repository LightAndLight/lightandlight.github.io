# `lightandlight.github.io`

<https://blog.ielliott.io>

My personal GitHub Pages site.

## Publishing

The actual site is served from [`docs/` on the `published`
branch](https://github.com/LightAndLight/lightandlight.github.io/tree/published/docs).

To publish the currently checked out branch, run `nix shell -c ./publish`. The
[`publish`](./publish) script syncs the relevant branches and generates the site, committing it to
`published` and pushing to GitHub. After pushing, you can see the status of the site deployment
[here](https://github.com/LightAndLight/lightandlight.github.io/actions). It takes about a minute
after pushing for the changes to show up on the blog.