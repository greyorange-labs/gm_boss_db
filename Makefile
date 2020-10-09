IGNORE_DEPS += edown eper eunit_formatters meck node_package rebar_lock_deps_plugin rebar_vsn_plugin reltool_util
C_SRC_DIR = /path/do/not/exist
C_SRC_TYPE = rebar
DRV_CFLAGS = -fPIC
export DRV_CFLAGS
ERLANG_ARCH = 64
export ERLANG_ARCH
ERLC_OPTS = +debug_info
export ERLC_OPTS
ERLC_OPTS += +'{parse_transform, lager_transform}'
ERLC_OPTS += +'{parse_transform, cut}'
ERLC_OPTS += +'{parse_transform, do}'
ERLC_OPTS += +'{parse_transform, import_as}'
ERLC_OPTS += +tuple_calls

DEPS += lager
dep_lager = git https://github.com/erlang-lager/lager.git 3.6.7
DEPS += erlando
dep_erlando = git https://github.com/ChicagoBoss/erlando.git 680688f
DEPS += aleppo
dep_aleppo = git https://github.com/ErlyORM/aleppo.git v0.9.4
DEPS += medici
dep_medici = git https://github.com/ErlyORM/medici.git bb6167459d
DEPS += ddb
dep_ddb = git https://github.com/ErlyORM/ddb.git v0.1.7
DEPS += epgsql
dep_epgsql = git https://github.com/epgsql/epgsql.git 4.2.0
DEPS += erlmc
dep_erlmc = git https://github.com/layerhq/erlmc.git c5280da
DEPS += mysql
dep_mysql = git https://github.com/ErlyORM/erlang-mysql-driver.git v0.0.4
DEPS += poolboy
dep_poolboy = git https://github.com/devinus/poolboy.git 1.5.2
DEPS += uuid
dep_uuid = git https://github.com/avtobiff/erlang-uuid.git v0.5.2
DEPS += redo
dep_redo = git https://github.com/heroku/redo.git cd75a11
DEPS += ets_cache
dep_ets_cache = git https://github.com/greyorange/ets_cache.git d30de6c1c5
DEPS += proper
dep_proper = git https://github.com/manopapad/proper.git v1.3
DEPS += dh_date
dep_dh_date = git https://github.com/daleharvey/dh_date.git 23e5a61
DEPS += tiny_pq
dep_tiny_pq = git https://github.com/ChicagoBoss/tiny_pq.git v0.9.0
DEPS += boss_test
dep_boss_test = git https://github.com/ChicagoBoss/boss_test.git 0.0.1


rebar_dep: preprocess pre-deps deps pre-app app

preprocess::

pre-deps::

pre-app::

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)