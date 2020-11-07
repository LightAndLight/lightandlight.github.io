---
layout: post
title: Docker Cleanup Commands
author: ielliott95
permalink: /docker-cleanup-commands/
date: 2020-11-07 12:55:00 +1000
tags:
    - programming
---

I've Googled this one too many times, so I'm writing it here for future reference.

```bash
#! /usr/bin/env bash

# remove containers
docker ps --all --format "{%raw%}{{.ID}}{%endraw%}" | xargs docker rm

# remove images
docker images --format "{%raw%}{{.ID}}{%endraw%}" | xargs docker rmi -f

# remove volumes
docker volume prune

# remove build cache
docker builder prune
```
