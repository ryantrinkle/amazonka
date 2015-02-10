SHELL     := /usr/bin/env bash
DEPS      := vendor/botocore vendor/jason
MODEL_DIR := model

CABAL_INSTALL_DEFARGS ?= -j --disable-documentation --disable-coverage

XML := \
 cloudfront \
 route53 \
 s3

QUERY := \
 autoscaling \
 cloudformation \
 cloudsearch \
 cloudwatch \
 ec2 \
 ecs \
 elasticache \
 elasticbeanstalk \
 elb \
 iam \
 importexport \
 rds \
 redshift \
 sdb \
 ses \
 sns \
 sqs \
 sts

JSON := \
 cloudhsm \
 cloudsearchdomain \
 cloudtrail \
 codedeploy \
 cognito-identity \
 cognito-sync \
 config \
 datapipeline \
 directconnect \
 dynamodb \
 elastictranscoder \
 emr \
 glacier \
 kinesis \
 kms \
 lambda \
 logs \
 opsworks \
 route53domains \
 storagegateway \
 support \
 swf

MODELS ?= $(QUERY) $(XML) $(JSON)

.PHONY: gen

gen: build
	@dist/build/amazonka-gen/amazonka-gen \
 --out=../ \
 --overrides=overrides \
 --templates=templates \
 --assets=assets \
 --retry=$(MODEL_DIR)/_retry.json \
 $(addprefix --model=,$(addprefix $(MODEL_DIR)/,$(MODELS)))

build: $(MODEL_DIR)
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: add-sources
	cabal install $(CABAL_INSTALL_DEFARGS) --only-dependencies

clean:
	cabal clean
	rm -rf $(MODEL_DIR) $(DEPS) cabal.sandbox.config .cabal-sandbox

add-sources: cabal.sandbox.config $(DEPS)
	cabal sandbox add-source vendor/jason

cabal.sandbox.config:
	cabal sandbox init

vendor/botocore:
	git clone git@github.com:boto/botocore $@

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@

$(MODEL_DIR):
	cp -rf vendor/botocore/botocore/data/aws $@
