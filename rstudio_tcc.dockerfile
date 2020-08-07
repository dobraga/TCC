FROM rocker/verse

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libudunits2-dev \
  libgdal-dev \
  libcurl4-openssl-dev \
  libssl-dev \
  clang \
  && install2.r --error \
  --deps TRUE \
  sf \
  tidymodels \
  patchwork \
  kableExtra \
  ggExtra \
  ranger \
  corrr \
  corrplot \
  car \
  caret \
  glmnet \
  skimr \
  flexdashboard \
  && tlmgr update --self \
  && tlmgr install \
  abntex2 \
  memoir \
  iftex \
  textcase
