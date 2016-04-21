create or replace package pkg_securities_ut is

  date_format  varchar2(100) := 'dd.mm.yyyy';
  test_precision number := 8;

  procedure ut_price(in_settlement in varchar2,
                     in_maturity   in varchar2,
                     in_rate       in number,
                     in_yield      in number,
                     in_redemption in number,
                     in_freuqency  in pls_integer,
                     in_basis      in pls_integer,
                     in_result     in number,
                     in_desc       in varchar2 default null);

  procedure ut_yield(in_settlement in varchar2,
                     in_maturity   in varchar2,
                     in_rate       in number,
                     in_price      in number,
                     in_redemption in number,
                     in_freuqency  in pls_integer,
                     in_basis      in pls_integer,
                     in_result     in number,
                     in_desc       in varchar2 default null);

  procedure init;

  procedure print_result;

end pkg_securities_ut;
/
create or replace package body pkg_securities_ut is
  g_test_count number := 0;
  g_errors     number;
  g_ok         number;
  ---------------------------------------------------------------------------------
  -- logging
  ---------------------------------------------------------------------------------
  procedure log(in_txt varchar2) is
  begin
    dbms_output.put_line(substr(in_txt, 1, 254));
  end;
  ---------------------------------------------------------------------------------
  -- check 
  ---------------------------------------------------------------------------------
  procedure test_assert(in_got_value in number, in_waited_value in number) is
    n_got  number := round(in_got_value,test_precision);
    n_wait number := round(in_waited_value,test_precision);
  begin
  
    log('------------------------------------------------');
    if nvl(n_got,-1) = nvl(n_wait,-2) then
      log('test(' || to_char(g_test_count) || ') PASSED');
      g_ok := g_ok + 1;
    else
      log('--(' || to_char(g_test_count) ||
          ')--------------------------------------  ERROR');
      g_errors := g_errors + 1;
    end if;
    log('waited value: ' || nvl(to_char(n_wait),'NULL'));
    log('got value:    ' || nvl(to_char(n_got),'NULL'));
    log('------------------------------------------------');
  exception
    when others then
      log('--(' || to_char(g_test_count) ||
          ')--------------------------------------  ERROR');
      log('ERR:' || SQLERRM);
      log(dbms_utility.format_error_backtrace);
  end test_assert;

  procedure init is
  begin
    g_errors := 0;
    g_ok     := 0;
  end;

  procedure print_result is
  begin
    log('------------------------------------------------');
    log('------------------------------------------------');
    log('Tests passed:'||to_char(g_ok));
    log('Tests with Error:'||to_char(g_errors));
    log('------------------------------------------------');
    log('------------------------------------------------');
  end;

  procedure test_log(in_type       in varchar2, --YIELD, PRICE
                     in_settlement in varchar2,
                     in_maturity   in varchar2,
                     in_rate       in number,
                     in_yield      in number,
                     in_price      in number,
                     in_redemption in number,
                     in_freuqency  in pls_integer,
                     in_basis      in pls_integer,
                     in_result     in number,
                     in_desc       in varchar2 default null) is
  begin
    g_test_count := g_test_count + 1;
    log('------------------------------------------------');
    log('test(' || to_char(g_test_count) || ','||in_type||') STARTED');
    log(rpad('in_settlement', 15, ' ') || in_settlement);
    log(rpad('in_maturity', 15, ' ') || in_maturity);
    log(rpad('in_rate', 15, ' ') || to_char(in_rate));
    if nvl(in_type, 'NULL') in ('NULL', 'PRICE') then
      log(rpad('in_yield', 15, ' ') || to_char(in_yield));
    end if;
    if nvl(in_type, 'NULL') in ('NULL', 'YIELD') then
      log(rpad('in_price', 15, ' ') || to_char(in_price));
    end if;
    log(rpad('in_redemption', 15, ' ') || to_char(in_redemption));
    log(rpad('in_freuqency', 15, ' ') || to_char(in_freuqency));
    log(rpad('in_basis', 15, ' ') || to_char(in_basis));
    log(rpad('in_result', 15, ' ') || to_char(in_result));
    log(rpad('in_desc', 15, ' ') || in_desc);
  end test_log;

  ---------------------------------------------------------------------------------
  -- check pkg_securities.price
  ---------------------------------------------------------------------------------
  procedure ut_price(in_settlement in varchar2,
                     in_maturity   in varchar2,
                     in_rate       in number,
                     in_yield      in number,
                     in_redemption in number,
                     in_freuqency  in pls_integer,
                     in_basis      in pls_integer,
                     in_result     in number,
                     in_desc       in varchar2 default null) is
  begin
    test_log('PRICE',
             in_settlement,
             in_maturity,
             in_rate,
             in_yield,
             null, --price
             in_redemption,
             in_freuqency,
             in_basis,
             in_result,
             in_desc);
  
    test_assert(pkg_securities.price(in_settlement => to_date(in_settlement,
                                                                    date_format),
                                           in_maturity   => to_date(in_maturity,
                                                                    date_format),
                                           in_rate       => in_rate,
                                           in_yield      => in_yield,
                                           in_redemption => in_redemption,
                                           in_frequency  => in_freuqency,
                                           in_basis      => in_basis),
                in_result);
  exception
    when others then
      test_assert(null, null);
      log('ERR:' || SQLERRM);
      log(dbms_utility.format_error_backtrace);
  end ut_price;

  procedure ut_yield(in_settlement in varchar2,
                     in_maturity   in varchar2,
                     in_rate       in number,
                     in_price      in number,
                     in_redemption in number,
                     in_freuqency  in pls_integer,
                     in_basis      in pls_integer,
                     in_result     in number,
                     in_desc       in varchar2 default null) is
  begin
  
    test_log('YIELD',
             in_settlement,
             in_maturity,
             in_rate,
             null, --in_yield,
             in_price,
             in_redemption,
             in_freuqency,
             in_basis,
             in_result,
             in_desc);
  
    test_assert(pkg_securities.yield(in_settlement => to_date(in_settlement,
                                                                    date_format),
                                           in_maturity   => to_date(in_maturity,
                                                                    date_format),
                                           in_rate       => in_rate,
                                           in_price      => in_price,
                                           in_redemption => in_redemption,
                                           in_frequency  => in_freuqency,
                                           in_basis      => in_basis),
                in_result);
  exception
    when others then
      test_assert(null, null);
      log('ERR:' || SQLERRM);
      log(dbms_utility.format_error_backtrace);
  end ut_yield;
begin

  g_errors := 0;
  g_ok     := 0;

end pkg_securities_ut;
/
