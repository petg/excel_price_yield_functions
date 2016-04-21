set serveroutput on
set define off
set echo off

spool deploy_xx.log

prompt pkg_securities.pck
@pkg_securities.pck
prompt pkg_securities_ut.pck
@pkg_securities_ut.pck

prompt ==========================================
prompt Deploy is done. Press return to run tests.
prompt ==========================================
prompt
pause 

@test.sql

spool off
prompt ===============================================
prompt Press return to exit. Check deploy_xx.log file.
prompt ===============================================
pause

exit