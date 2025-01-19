# `lightandlight.github.io`

<https://blog.ielliott.io>

My personal GitHub Pages site.

## Publishing

The actual site is served from [`docs/` on the `published`
branch](https://github.com/LightAndLight/lightandlight.github.io/tree/published/docs).

To publish the currently checked out branch, run `nix develop -c ./publish`. The
[`publish`](./publish) script syncs the relevant branches and generates the site, committing it to
`published` and pushing to GitHub. After pushing, you can see the status of the site deployment
[here](https://github.com/LightAndLight/lightandlight.github.io/actions). It takes about a minute
after pushing for the changes to show up on the blog.

## Features

* [Post front-matter](#post-front-matter)
* [Tag pages](#tag-pages)
* [Math support](#math-support)
* [Table of contents generation](#table-of-contents-generation)
* [Asciinema](#asciinema)

### Post front-matter

| Field | Required? | Type | Comments |
|---|---|---|---|
| `title` | yes | string | Used in the HTML `<title>`, and displayed as the post's heading. |
| `permalink` | yes | string | The post's absolute path on the site. |
| `date` | no | string (format: YYYY-mm-dd) | The published date. Appears under the post's heading. If omitted, the post's filepath must begin with the published date. |
| `excerpt` | no | string (format: HTML) | A summary of the post. Displayed on the post index. When omitted, the post's first paragraph is used. |
| `tags` | no | list of string | Shown for each post on the post index, and at the bottom of a post's page. See also: [Tag pages](#tag-pages). |
| `math` | no | bool | Enable [Math support](#math-support) |
| `chinese` | no | bool | Use a better font for Chinese characters |
| `asciinema` | no | bool | Enable [Asciinema](#asciinema) |
| `feed_id` | no | string | Used for feed compatibility. A post's feed ID is based on its `permalink`, so if you change the permalink then feed readers will think you wrote a new post. If you change a `permalink`, set the post's `feed_id` to whatever it was when it was originally published. |

### Tag pages

Every tag named in a `tags` [front-matter](#post-front-matter) field gets its own tags page at `/tag/{tagName}`.
Each tags page lists all the posts with that tag. 

### Math support

When the [post front-matter](#post-front-matter) has `math: true`, the following is enabled for a post's page:

* Better MathML font ([Libertinus Math](https://github.com/alerque/libertinus))
* MathML-specific CSS
* MathJax-based polyfill when MathML isn't supported

### Table of contents generation

If the placeholder element `<div id="toc"><!-- generated --></div>` is detected in a post, then the element's content will be set to the post's table of contents.

#### Customisation

By default, the table of contents includes all headings and subheadings beginning at header level 2 (header level 1 is the post's title).
This can be overridden using [header attributes](https://pandoc.org/MANUAL.html#extension-header_attributes).

<table>
<thead>
<tr>
<th>Attribute</th>
<th>Type</th>
<th>Effect</th>
<th>Example</th>
</thead>
<tbody>
<tr>
<td>

`toc:omit_children`

</td>
<td>bool</td>
<td>Omit all of this heading's subheadings from the table of contents</td>
<td>

```
## A heading {toc:omit_children=true}

The above heading is included in the TOC, but its subheadings are not.

### A subheading

The above heading is omitted.

### Another subheading

So is the above.

## A different heading

### A different subheading

The above subheading is included, because its parent hasn't set `toc:omit_children`.
```

</td>
</tr>
</tbody>
</table>

### Asciinema

When the [post front-matter](#post-front-matter) has `asciinema: true`,
[Asciinema](https://asciinema.org) casts can be embedded using the `<x-asciinema-cast>` element.

Example:

```html
<x-asciinema-cast data-src="/path/to/cast"></x-asciinema-cast>
```

