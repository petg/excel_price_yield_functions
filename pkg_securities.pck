create or replace package pkg_securities is

  log_enable boolean := false;


  function price(in_settlement in date,
                 in_maturity   in date,
                 in_rate       in number,
                 in_yield      in number,
                 in_redemption in number,
                 in_frequency  in pls_integer,
                 in_basis      in pls_integer default 0) return number;

  function yield(in_settlement in date,
                 in_maturity   in date,
                 in_rate       in number,
                 in_price      in number,
                 in_redemption in number,
                 in_frequency  in pls_integer,
                 in_basis      in pls_integer default 0) return number;

end pkg_securities;
/
create or replace package body pkg_securities is

  /*
  price
  Settlement    Required. The security's settlement date. The security settlement date is the date after the issue date when the security is traded to the buyer.
  Maturity    Required. The security's maturity date. The maturity date is the date when the security expires.
  Rate    Required. The security's annual coupon rate.
  Yld    Required. The security's annual yield.
  Redemption    Required. The security's redemption value per $100 face value.
  Frequency    Required. The number of coupon payments per year. For annual payments, frequency = 1; for semiannual, frequency = 2; for quarterly, frequency = 4.
  Basis    Optional. The type of day count basis to use.
  
  DSC = number of days from settlement to next coupon date.
  E = number of days in coupon period in which the settlement date falls.
  A = number of days from beginning of coupon period to settlement date.
  */

  exc_in_assert exception;
  n_exc_in_assert pls_integer := -20201;
  PRAGMA EXCEPTION_INIT(exc_in_assert, -20201);

  type t_r_date_part is record(
    dd   pls_integer,
    mm   pls_integer,
    yyyy pls_integer);

  type t_dates is record(
    settlement_date            date,
    maturity_date              date,
    frequency                  number,
    all_days                   number,
    months_per_coupon          number,
    days_per_coupon            number,
    coupons_count              number,
    prev_coupon_date           date,
    next_coupon_date           date,
    basis_year_days            number,
    sttl2first_coupon_days_DST number,
    zero_coupon2sttl_daysA     number,
    accrue_fraction            number,
    DSC_fraction                 number);

  ---------------------------------------------------------------------------------
  -- logging
  ---------------------------------------------------------------------------------
  procedure log(in_txt varchar2) is
  begin
    if log_enable then
      dbms_output.put_line(substr(in_txt, 1, 254));
    end if;
  end;

  procedure logn(t varchar2, n number) is
  begin
    log(t || to_char(round(n, 2)));
  end;
  ---------------------------------------------------------------------------------
  -- check 
  ---------------------------------------------------------------------------------
  procedure assert(in_true_condition in boolean default true,
                   in_err_msg        in varchar2) is
  begin
    if not in_true_condition then
      raise_application_error(n_exc_in_assert, in_err_msg);
    end if;
  end assert;
  ---------------------------------------------------------------------------------
  -- p_t_dates_print
  ---------------------------------------------------------------------------------
  procedure p_t_dates_print(in_t_dates t_dates) is
  begin
    log(' --- t_dates --- ');
    log(rpad('settlement_date', 30, ' ') || ':' ||
        to_char(in_t_dates.settlement_date));
    log(rpad('maturity_date', 30, ' ') || ':' ||
        to_char(in_t_dates.maturity_date));
    log(rpad('frequency', 30, ' ') || ':' || to_char(in_t_dates.frequency));
    log(rpad('all_days', 30, ' ') || ':' || to_char(in_t_dates.all_days));
    log(rpad('months_per_coupon', 30, ' ') || ':' ||
        to_char(in_t_dates.months_per_coupon));
    log(rpad('days_per_coupon', 30, ' ') || ':' ||
        to_char(in_t_dates.days_per_coupon));
    log(rpad('coupons_count', 30, ' ') || ':' ||
        to_char(in_t_dates.coupons_count));
    log(rpad('prev_coupon_date', 30, ' ') || ':' ||
        to_char(in_t_dates.prev_coupon_date));
    log(rpad('next_coupon_date', 30, ' ') || ':' ||
        to_char(in_t_dates.next_coupon_date));
    log(rpad('basis_year_days', 30, ' ') || ':' ||
        to_char(in_t_dates.basis_year_days));
    log(rpad('zero_coupon2sttl_daysA', 30, ' ') || ':' ||
        to_char(in_t_dates.zero_coupon2sttl_daysA));
    log(rpad('sttl2first_coupon_days_DST', 30, ' ') || ':' ||
        to_char(in_t_dates.sttl2first_coupon_days_DST));
    log(rpad('accrue_fraction', 30, ' ') || ':' ||
        to_char(in_t_dates.accrue_fraction));
    log(rpad('DSC_fraction', 30, ' ') || ':' ||
        to_char(in_t_dates.DSC_fraction));
    log(' --- ------- --- ');
  end;
  ---------------------------------------------------------------------------------
  -- f_get_date_parts
  ---------------------------------------------------------------------------------
  function init_dates(in_settlement in date,
                      in_maturity   in date,
                      in_frequency  in pls_integer) return t_dates is
    lt_price_dates t_dates;
  begin
    log('in_settlement:' || to_char(in_settlement) || '; in_maturity:' ||
        to_char(in_maturity) || '; in_frequency:' || to_char(in_frequency));
    assert(in_settlement is not null and in_maturity is not null and
           in_frequency is not null,
           'in_date is not null');
  
    lt_price_dates.settlement_date   := in_settlement;
    lt_price_dates.maturity_date     := in_maturity;
    lt_price_dates.all_days          := in_maturity - in_settlement;
    lt_price_dates.frequency         := in_frequency;
    lt_price_dates.months_per_coupon := 12 / in_frequency;
    -- p_t_dates_print(lt_price_dates);
    return lt_price_dates;
  end;


  function f_get_first_date_next_month(in_d date) return date
  is
  begin
    return add_months(to_date('01'||to_char(in_d,'mm.yyyy'),'dd.mm.yyyy'), 1) - 1;
  end;
  
  function f_get_month_last_day(d1 date) return boolean is
    d date;
  begin
    d := f_get_first_date_next_month(d1);
    return trim(d1) = trim(d);
  end;


  procedure p_get_first_coupons(in_t_dates in out nocopy t_dates) is
    ln_coupon_number    number := 0;
    ld_prev_coupon_date date := in_t_dates.maturity_date;
    ld_next_coupon_date date := in_t_dates.settlement_date;
  begin
  
    while (ld_prev_coupon_date > in_t_dates.settlement_date) loop
      ln_coupon_number    := ln_coupon_number + 1;
      ld_next_coupon_date := ld_prev_coupon_date;
      ld_prev_coupon_date := add_months(ld_prev_coupon_date,
                                        -1 * in_t_dates.months_per_coupon);
    end loop;

    if f_get_month_last_day(in_t_dates.maturity_date) and
       not f_get_month_last_day(ld_prev_coupon_date)
    then
      ld_prev_coupon_date := f_get_first_date_next_month(ld_prev_coupon_date);
    end if;
  
    in_t_dates.prev_coupon_date := ld_prev_coupon_date;
    in_t_dates.next_coupon_date := ld_next_coupon_date;
    in_t_dates.coupons_count    := ln_coupon_number;

    --  p_t_dates_print(in_t_dates);
  end;

  ---------------------------------------------------------------------------------
  -- f_get_date_parts
  ---------------------------------------------------------------------------------
  function f_get_date_parts(in_date in date) return t_r_date_part is
    r_date t_r_date_part;
  begin
    assert(in_date is not null, 'in_date is not null');
    r_date.dd   := to_number(to_char(in_date, 'dd'));
    r_date.mm   := to_number(to_char(in_date, 'mm'));
    r_date.yyyy := to_number(to_char(in_date, 'yyyy'));
  
    return r_date;
  end f_get_date_parts;

  function f_get_year_days(in_date date) return number is
    n_days number;
    n_yy   number;
  begin
    n_yy   := to_number(to_char(in_date, 'yyyy'));
    n_days := to_date('01.01.' || to_char(n_yy + 1), 'dd.mm.yyyy') -
              to_date('01.01.' || to_char(n_yy), 'dd.mm.yyyy');
    return n_days;
  end;

  ---------------------------------------------------------------------------------
  -- get days by basis 
  ---------------------------------------------------------------------------------

  procedure p_get_days_by_basis(in_basis   in pls_integer,
                                in_t_dates in out nocopy t_dates) is
    n_basis number := nvl(in_basis, 0);
    r_date1 t_r_date_part;
    r_date2 t_r_date_part;
    date1   date;
    date2   date;
    c1 date;
    c2 date;
  begin
    log('p_get_days_by_basis:start:in_basis=' || to_char(in_basis));
    assert(n_basis in (0, 1, 2, 3, 4), 'Basis in (0,1,2,3,4)');
  
    date1   := in_t_dates.prev_coupon_date;
    date2   := in_t_dates.settlement_date;
    r_date1 := f_get_date_parts(date1);
    r_date2 := f_get_date_parts(date2);
  
    if n_basis = 0 then
      --30\360 US
      if r_date1.mm = 2 and f_get_month_last_day(date1) then
        r_date1.dd := 30;
      end if;
    
      if r_date1.mm = 2 and f_get_month_last_day(date1) and r_date2.mm = 2 and
         f_get_month_last_day(date2) then
        r_date2.dd := 30;
      end if;
    
      if r_date1.dd = 31 then
        r_date1.dd := 30;
      end if;
    
      if r_date2.dd = 31 and r_date1.dd >= 30 then
        r_date2.dd := 30;
      end if;
      
      in_t_dates.zero_coupon2sttl_daysA := (r_date2.dd - r_date1.dd) +
                                               (30 *
                                               (r_date2.mm - r_date1.mm)) +
                                               360 *
                                               (r_date2.yyyy - r_date1.yyyy);
                                               
      in_t_dates.basis_year_days            := 360;
    
      in_t_dates.days_per_coupon := in_t_dates.basis_year_days /
                                    in_t_dates.frequency;

      in_t_dates.accrue_fraction := in_t_dates.zero_coupon2sttl_daysA / in_t_dates.days_per_coupon;
      
      in_t_dates.sttl2first_coupon_days_DST := in_t_dates.days_per_coupon -
                                         in_t_dates.zero_coupon2sttl_daysA;   
    
    elsif n_basis = 1 then
      --ACT\ACT
      in_t_dates.sttl2first_coupon_days_DST := date2 - date1;
      in_t_dates.basis_year_days            := f_get_year_days(in_t_dates.next_coupon_date);
      in_t_dates.days_per_coupon            := in_t_dates.basis_year_days /
                                               in_t_dates.frequency;

      if in_t_dates.next_coupon_date > in_t_dates.settlement_date then
      
        c1 := in_t_dates.prev_coupon_date;
        c2 := in_t_dates.next_coupon_date;
        
      else
        c1 := in_t_dates.next_coupon_date;
        c2 := in_t_dates.settlement_date;
      
      end if;
        
      in_t_dates.accrue_fraction := (in_t_dates.settlement_date - in_t_dates.prev_coupon_date)/(c2-c1);
                                               
    elsif n_basis = 2 then
      --ACT\360
      in_t_dates.sttl2first_coupon_days_DST := date2 - date1;
      in_t_dates.basis_year_days            := 360;
      in_t_dates.days_per_coupon            := in_t_dates.basis_year_days /
                                               in_t_dates.frequency;
      
      in_t_dates.accrue_fraction := (in_t_dates.settlement_date - in_t_dates.prev_coupon_date)/in_t_dates.days_per_coupon;
      
                                 
    elsif n_basis = 3 then
      --ACT\365
      in_t_dates.sttl2first_coupon_days_DST := date2 - date1;
      in_t_dates.basis_year_days            := 365;
      in_t_dates.days_per_coupon            := in_t_dates.basis_year_days /
                                               in_t_dates.frequency;

      in_t_dates.accrue_fraction := (in_t_dates.settlement_date - in_t_dates.prev_coupon_date)/in_t_dates.days_per_coupon;
                                 
    elsif n_basis = 4 then
      --30E\360 EUR
    
      if r_date1.dd = 31 then
        r_date1.dd := 30;
      end if;
    
      if r_date2.dd = 31 then
        r_date2.dd := 30;
      end if;
    
      in_t_dates.zero_coupon2sttl_daysA := (r_date2.dd - r_date1.dd) +
                                               (30 *
                                               (r_date2.mm - r_date1.mm)) +
                                               360 *
                                               (r_date2.yyyy - r_date1.yyyy);
      in_t_dates.basis_year_days            := 360;
    
      in_t_dates.days_per_coupon := in_t_dates.basis_year_days /
                                    in_t_dates.frequency;
                                    
      in_t_dates.accrue_fraction := in_t_dates.zero_coupon2sttl_daysA / in_t_dates.days_per_coupon;
      
      in_t_dates.sttl2first_coupon_days_DST := in_t_dates.days_per_coupon -
                                         in_t_dates.zero_coupon2sttl_daysA;    
    end if;
    
    in_t_dates.DSC_fraction :=   1 - in_t_dates.accrue_fraction;

  
    p_t_dates_print(in_t_dates);
  end p_get_days_by_basis;
  ---------------------------------------------------------------------------------
  --   PRICE
  ---------------------------------------------------------------------------------
  function price(in_settlement in date,
                 in_maturity   in date,
                 in_rate       in number,
                 in_yield      in number,
                 in_redemption in number,
                 in_frequency  in pls_integer,
                 in_basis      in pls_integer default 0) return number is
    n_coupon_yield      number := (in_yield / in_frequency) / 100;
    n_coupon_rate       number := (in_rate / in_frequency) / 100;
    n_power_int         number;
    n_power_int_circle  number;
    n_accrue_interest   number;
    lt_price_dates      t_dates;
    n_redemption_value  number := 0;
    n_coupon_interest   number := 0;
    n_result            number := 0;
    t1                  number := 0;
    t2                  number := 0;
  begin
    assert(in_settlement is not null and in_maturity is not null and
           in_rate is not null and in_yield is not null and
           in_redemption is not null and in_frequency is not null and
           in_basis is not null,
           'Parameters are not null.');
    assert(in_settlement < in_maturity, 'Incorrect dates.');
    assert(in_rate >= 0 or in_yield >= 0 or in_redemption > 0,
           'rate and yield >= 0 redemption > 0');
    assert(in_frequency in (1, 2, 4), 'only year, half year or quoter');
    assert(in_basis in (0, 1, 2, 3, 4), 'basis in 1 - 4');
  
    lt_price_dates := init_dates(in_settlement, in_maturity, in_frequency);
  
    p_get_first_coupons(lt_price_dates);
  
    p_get_days_by_basis(in_basis => in_basis, in_t_dates => lt_price_dates);
  
    
  
    if lt_price_dates.coupons_count > 0 then
      ----------------------------------------------------------------------------
      n_power_int        := lt_price_dates.coupons_count - 1 +
                            lt_price_dates.DSC_fraction;
      n_redemption_value := in_redemption /
                            power((1 + n_coupon_yield), n_power_int);
      ----------------------------------------------------------------------------
      n_coupon_interest := 0;
      for i in 1 .. lt_price_dates.coupons_count loop
      
        n_power_int_circle := ((i - 1) + lt_price_dates.DSC_fraction);
      
        n_coupon_interest := n_coupon_interest +
                             ((100 * n_coupon_rate) /
                             power((1 + n_coupon_yield),
                                    n_power_int_circle));
      end loop;
      n_result := n_redemption_value + n_coupon_interest;
    else
      t1       := 100 * n_coupon_rate + in_redemption;
      t2       := n_coupon_yield * lt_price_dates.DSC_fraction + 1;
      n_result := t1 / t2;
    end if;
  
    ----------------------------------------------------------------------------
    n_accrue_interest := ((100 * n_coupon_rate) *
                         lt_price_dates.accrue_fraction);
    ----------------------------------------------------------------------------
    n_result := n_result - n_accrue_interest;
  
    return n_result;
  exception
    when others then
      return - 1;
  end price;

  ---------------------------------------------------------------------------------
  --   D_PRICE
  ---------------------------------------------------------------------------------
  function d_price(in_settlement in date,
                   in_maturity   in date,
                   in_rate       in number,
                   in_yield      in number,
                   in_redemption in number,
                   in_frequency  in pls_integer,
                   in_basis      in pls_integer default 0) return number is
    n_coupon_yield     number := (in_yield / in_frequency) / 100;
    n_coupon_rate      number := (in_rate / in_frequency) / 100;
    lt_price_dates     t_dates;
    q                  number;
    g                  number;
    a                  number;
    b                  number;
    c                  number;
    p1                 number;
    p2                 number;
  begin
    lt_price_dates := init_dates(in_settlement, in_maturity, in_frequency);
    p_get_first_coupons(lt_price_dates);
    p_get_days_by_basis(in_basis => in_basis, in_t_dates => lt_price_dates);
  
    q := 1 + (n_coupon_yield);
    g := lt_price_dates.coupons_count - 1 + (lt_price_dates.sttl2first_coupon_days_DST /
         lt_price_dates.days_per_coupon);
  
    p1 := (in_redemption * (-1) * g) / power(q, -1 * (g + 1));
  
    a := 100 * n_coupon_rate;
    b := 1 + (n_coupon_yield);
    c := (lt_price_dates.sttl2first_coupon_days_DST /
         lt_price_dates.days_per_coupon) - 1;
  
    p2 := 0;
    for k in 1 .. lt_price_dates.coupons_count loop
    
      p2 := p2 + ((-1) * (k + c) * a) / power(b, k + c + 1);
    
    end loop;
  
    return p1 + p2 + 0 /* p3 */
    ;
  
  exception
    when others then
      return - 1;
  end d_price;

  function yield(in_settlement in date,
                 in_maturity   in date,
                 in_rate       in number,
                 in_price      in number,
                 in_redemption in number,
                 in_frequency  in pls_integer,
                 in_basis      in pls_integer default 0) return number is
    lt_yield_dates    t_dates;
    i                 number;
    fy0_2             number;
    y0_2              number;
    dfy0_2            number;
    y1_2              number;
    n_redemption_rate number := in_redemption / 100;
    n_coupon_rate     number := in_rate / 100;
    n_price_rate      number := in_price / 100;
    n_accrue_interest number;
    n_yield_rate      number;
  begin
    lt_yield_dates := init_dates(in_settlement, in_maturity, in_frequency);
    p_get_first_coupons(lt_yield_dates);
    p_get_days_by_basis(in_basis => in_basis, in_t_dates => lt_yield_dates);
  
    n_accrue_interest := (lt_yield_dates.accrue_fraction) * n_coupon_rate;
  
    if lt_yield_dates.coupons_count <= 1 then
      n_yield_rate := (((n_redemption_rate + n_coupon_rate) -
                      (n_price_rate + n_accrue_interest)) /
                      (n_price_rate + n_accrue_interest)) *
                      (in_frequency *
                      (lt_yield_dates.days_per_coupon /
                      lt_yield_dates.sttl2first_coupon_days_DST));
      log('--------------- yield ------------ 0');
      log('n_yield_rate:' || round(n_yield_rate * 100, 2));
      log('----------------------------------');
      return n_yield_rate /** 100*/;
    end if;

    y0_2 := (in_rate / 100);
    --y0_2 := n_yield_rate;
  
    i := 0;
    while (i <= 600) loop
      log('---------------------------------------' || to_char(i));
      i      := i + 1;
      fy0_2  := price(in_settlement,
                      in_maturity,
                      in_rate,
                      y0_2 * 100,
                      in_redemption,
                      in_frequency,
                      in_basis) - in_price;
      dfy0_2 := d_price(in_settlement,
                        in_maturity,
                        in_rate,
                        y0_2 * 100,
                        in_redemption,
                        in_frequency,
                        in_basis);
      logn('in_rate:', in_rate);
      logn('y0_2(yield):', y0_2 * 100);
      logn('fy0_2:', fy0_2);
      logn('dfy0_2:', dfy0_2);
      log('(fy0_2 / dfy0_2):' || to_char(fy0_2 / dfy0_2));
      if fy0_2 < 0 then
        y1_2 := y0_2 - abs(fy0_2 / dfy0_2);
      else
        y1_2 := y0_2 + abs(fy0_2 / dfy0_2);
      end if;
    
      logn('y1_2:', y1_2 * 100);
      y0_2 := y1_2;
    
    end loop;
    return y0_2/* * 100*/;
  exception
    when others then
      return - 1;
  end;

end pkg_securities;
/
