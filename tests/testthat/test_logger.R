# Copyright (C) 2018  Jochen Weile, Roth Lab
#
# This file is part of YogiLog.
#
# YogiLog is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# YogiLog is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with YogiLog.  If not, see <https://www.gnu.org/licenses/>.

library("yogilog")

context("logging")

test_that("logger works",{

	#allocate log file
	lfile <- tempfile("loggertest")

	logger <- new.logger(lfile)
	
	testmsg <- "infotest"
	logger$info(testmsg)

	writtenLines <- scan(lfile,what="character",sep="\n")
	expect_true(any(grepl(testmsg,writtenLines)),"test message should be in log file")

	logger <- new.logger(lfile,stdout=FALSE)
	
	testmsg <- "silenttest"
	logger$info(testmsg)

	writtenLines <- scan(lfile,what="character",sep="\n")
	expect_true(any(grepl(testmsg,writtenLines)),"test message should be in log file")

	#clean up
	file.remove(lfile)

})