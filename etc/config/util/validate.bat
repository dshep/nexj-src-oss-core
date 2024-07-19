@echo off
setlocal enabledelayedexpansion
IF NOT EXIST build.xml GOTO missing_build_files
IF NOT EXIST build.properties GOTO missing_build_files
GOTO check_libraries

:check_libraries
IF "%JAVA_HOME%"=="" GOTO missing_java
dir /B %JAVA_HOME%\jre\lib\ext\mysql-connector-java*.jar >ext_list 2>NUL
set MYSQLJ=""
FOR /F "tokens=* delims=" %%i IN ('findstr -r "mysql-connector-java.*jar" ext_list') DO set MYSQLJ="%%i"
del ext_list
IF %MYSQLJ%=="" GOTO missing_mysql_connectorj
set THIRD_DIR=""
FOR /F "tokens=1,2 delims==" %%i IN (build.properties) DO IF %%i==3rd.dir set THIRD_DIR=%%j
IF %THIRD_DIR%=="" GOTO missing_3rd
SET THIRD_DIR=%THIRD_DIR:/=\%
IF EXIST %THIRD_DIR%\apache-ant-1.8.2\bin\ant.cmd (call ant init.3rd.base 2>ant_return >NUL) ELSE GOTO missing_ant
FOR /F "tokens=* delims=" %%i IN ('findstr -r ".*BUILD FAILED.*" ant_return') DO GOTO missing_library
del ant_return
echo Libraries installation checking is successful.
GOTO check_mysql

:check_mysql
set ENV_FILE=""
FOR /F "tokens=1,2 delims==" %%i IN (build.properties) DO IF %%i==server set ENV_FILE=%%j
IF %ENV_FILE%=="" GOTO missing_env
set ENV_FILE=%ENV_FILE:/=\%
set DB_LOCATION=""
FOR /F "tokens=* delims=" %%i IN ('findstr -r ".*MySQL.*host=.*port=.*" %ENV_FILE%') DO set DB_LOCATION="%%i"
IF %DB_LOCATION%=="" GOTO missing_mysql_config
set DB_LOCATION=%DB_LOCATION:<=%
set DB_LOCATION=%DB_LOCATION:/>=%
set HOST=%DB_LOCATION:* host="%
set PORT=%DB_LOCATION:* port="%
FOR /F "tokens=1,2 delims== " %%i IN ('echo %HOST%') DO set HOST=%%~j
FOR /F "tokens=1,2 delims== " %%i IN ('echo %PORT%') DO set PORT=%%~j
echo quit > quit.sql
call mysql -h%HOST% -P%PORT% < quit.sql 2> db_return
set DB_RESULT=""
FOR /F "tokens=* delims=" %%i IN ('findstr -r ".*ERROR.*" db_return') DO set DB_RESULT="%%i"
IF %DB_RESULT%=="" GOTO success
set DB_RESULT=""
FOR /F "tokens=* delims=" %%i IN ('findstr -r ".*ERROR\ 1045.*" db_return') DO set DB_RESULT="%%i"
IF %DB_RESULT%=="" GOTO mysql_env_config
GOTO success

:missing_java
echo JAVA_HOME is not defined. Check your JDK installation.
GOTO end

:missing_mysql_connectorj
echo MySQL Connector/J cannot be found in %JAVA_HOME%\jre\lib\ext.
GOTO end

:missing_build_files
echo Cannot find build.xml and build.properties in current directory.
GOTO end

:missing_3rd
echo 3rd.dir is not defined in build.properties.
GOTO end

:missing_env
echo Cannot find environment file %ENV_FILE%.
GOTO end

:missing_mysql_config
del db_return quit.sql
echo Cannot find MySQL server info in %ENV_FILE%.
GOTO end

:mysql_env_config
echo Cannot connect MySQL server on host:%HOST% port:%PORT%.
echo Please check your MySQL configration in %ENV_FILE%.
GOTO end

:missing_ant
echo Apache Ant 1.8.2 is not found in %THIRD_DIR%\apache-ant-1.8.2
GOTO end

:missing_library
FOR /F "tokens=* delims=" %%i IN (ant_return) DO (echo %%i)
del ant_return
GOTO end

:success
del db_return quit.sql
echo MySQL configration checking is successful.
GOTO end

:end
endlocal
