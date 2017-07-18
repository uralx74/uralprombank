create or replace package pk_nasel_sweety is

  -- Author  : V.OVCHINNIKOV
  -- Created : 22.03.2017 14:43:43
  -- Purpose : Package for Sweety application
  
  -- type refcursor is REF CURSOR;
  
  -- Public type declarations
  /*type <TypeName> is <Datatype>;
  
  -- Public constant declarations
  <ConstantName> constant <Datatype> := <Value>;

  -- Public variable declarations
  <VariableName> <Datatype>;

  -- Public function and procedure declarations
  function <FunctionName>(<Parameter> <Datatype>) return <Datatype>;*/
    
  v_user_app_ver_dt date;
  
  /*function is_test(p_fa_pack_id in nasel_fa_pack.fa_pack_id%type)
  return nasel_fa_pack.fa_pack_id%type;*/
    /* ��������� ��� ��������� ��� ������ � �������������
  */
  
  -- ��� ��� �������� ������ �������������
  type t_char_list is table of varchar2(2000) index by varchar2(8);
    
  type t_fa_id_list is table of varchar2(10) index by binary_integer; -- ������ fa_id 
  type t_acct_id_list is table of varchar2(10) index by binary_integer;
  --type t_acct_id_list is table of nasel_ccb_prem.acct_id%type index by binary_integer;
    
  function get_fp_char_list (p_fa_pack_id in nasel_fa_pack_char.fa_pack_id%type)
    return t_char_list;
    
  /*procedure get_fa_pack_char_list1 (p_fa_pack_id in nasel_fa_pack_char.fa_pack_id%type -- ����� �������
  , rc out sys_refcursor);*/
  
  
  procedure get_fp_stats(p_acct_otdelen nasel_ccb_prem.acct_otdelen%type, p_start_dt date, p_end_dt date, rc out sys_refcursor);


  -- ��������� �����
  function add_fp(
    p_fa_pack_type_cd  IN nasel_fa_pack.fa_pack_type_cd%type,
    p_prnt_fa_pack_id  IN nasel_fa_pack.prnt_fa_pack_id%type default null ,
    p_acct_otdelen in nasel_fa_pack.acct_otdelen%type
  )
  return nasel_fa_pack.fa_pack_id%type; 

  function create_fp(
    p_fa_pack_type_cd  in nasel_fa_pack.fa_pack_type_cd%type,
    p_prnt_fa_pack_id  in nasel_fa_pack.prnt_fa_pack_id%type default null ,
    p_acct_otdelen in nasel_fa_pack.acct_otdelen%type
  )
  return nasel_fa_pack.fa_pack_id%type; 
  
  --function get_fp_char_list (p_fa_pack_id in nasel_fa_pack_char.fa_pack_id%type) return t_char_list;                     
                       
  /*procedure add_fa_pack_char_recipient(
    p_fa_pack_id in nasel_fa_pack_char.fa_pack_id%type,
    p_rt_spr_cd in nasel_fa_pack_char.char_val_str%type,        -- ��� �����������
    p_rt_spr in nasel_fa_pack_char.char_val_str%type,
    p_rt_addr in nasel_fa_pack_char.char_val_str%type,
    p_rt_post in nasel_fa_pack_char.char_val_str%type,
    p_rt_name in nasel_fa_pack_char.char_val_str%type
  );*/
  
  --procedure update_fa_pack_char_recipient(p_fa_pack_id in nasel_fa_pack_char.fa_pack_id%type);
 procedure add_fp_char_recipient(p_fa_pack_id in nasel_fa_pack_char.fa_pack_id%type, p_force_self integer default null);

     
  procedure set_fp_status_flg(
    p_fa_pack_id IN nasel_fa_pack_char.fa_pack_id%type,
    p_fa_pack_status_flg in nasel_fa_pack.fa_pack_status_flg%type
  );

  -- 
  procedure set_fp_status_flg_frozen(
    p_fa_pack_id in nasel_fa_pack.fa_pack_id%type
  );
  --procedure set_fa_pack_status_flg_incomp(  p_fa_pack_id in nasel_fa_pack.fa_pack_id%type);
  
  procedure set_fp_status_flg_cancel(
    p_fa_pack_id in nasel_fa_pack.fa_pack_id%type
  );

  procedure set_fp_stat_sent_perf(
    p_fa_pack_id in nasel_fa_pack.fa_pack_id%type
  );
  
  -- ��������� �������������� ��� �������
  procedure add_fp_char_str(
    p_fa_pack_id in nasel_fa_pack_char.fa_pack_id%type,
    p_fa_pack_char_type_cd in nasel_fa_pack_char.char_type_cd%type,
    p_fa_pack_char_val_str in nasel_fa_pack_char.char_val_str%type
  );

  -- ��������� �������������� ��� �������
  procedure add_fp_char_dttm(
    p_fa_pack_id in nasel_fa_pack_char.fa_pack_id%type,
    p_fa_pack_char_type_cd in nasel_fa_pack_char.char_type_cd%type,
    p_fa_pack_char_val_dttm in nasel_fa_pack_char.char_val_dttm%type
  );

  -- ������������� ���� ��������� ��� ������������ fa
 /* procedure set_fa_status_flg(
    p_fa_id in nasel_fa.fa_id%type,
    p_fa_status_flg in nasel_fa.fa_status_flg%type
  );*/

  -- ��������� ������� � ������ (������ �������)
  function create_fa_stg (
    p_acct_id  in nasel_fa.acct_id%type,
    p_fa_pack_id in nasel_fa.fa_pack_id%type,
    p_saldo_uch in nasel_fa.saldo_uch%type,
    p_mtr_serial_nbr in nasel_fa.mtr_serial_nbr%type,
    p_end_reg_reading1 in nasel_fa.end_reg_reading1%type,
    p_end_reg_reading2 in nasel_fa.end_reg_reading2%type
    --p_fa_status_flg in nasel_fa.fa_status_flg%type
  ) return nasel_fa.fa_id%type; 

  procedure create_fp_stop(p_acct_id_list in t_acct_id_list, p_force_self number default 0);
  procedure create_fp_stg (acct_otdelen nasel_fa_pack.acct_otdelen%type);--

  -- ��������� ������� � ������
  -- � ������� ������������� create_fa_notice 
  -- ������� ����������� ��������
  function create_fa (
    p_acct_id  in nasel_fa.acct_id%type,
    p_fa_pack_id in nasel_fa.fa_pack_id%type,
    p_prnt_fa_id in nasel_fa.prnt_fa_id%type default null
  ) return nasel_fa.fa_id%type; 

  -- ������� ������ �� ����������
  function create_fa_stop (
    p_acct_id  in nasel_fa.acct_id%type,
    p_fa_pack_id in nasel_fa.fa_pack_id%type
  ) return nasel_fa.fa_id%type; 


  -- ��������� ������������� 'ST-P-DT' (�������� ���� ����� �����������) 
  -- � ������� ������������� nasel_fa_char
  procedure set_fa_char_st_p_dt (
    p_fa_id nasel_fa.fa_id%type
  );

  -- ��������� ������� ���� � �����                               
  /*procedure add_fa_to_pack_stop (
    p_acct_id  IN nasel_fa.acct_id%type,
    p_fa_pack_id  IN nasel_fa.fa_pack_id%type
  );*/


  -- ��������� �������������� � FA  
  procedure add_fa_char_str(
    p_fa_id IN nasel_fa_char.fa_id%type,
    p_fa_char_type_cd in nasel_fa_char.char_type_cd%type,
    p_fa_char_val_str in nasel_fa_char.char_val_str%type
  );
  -- ��������� �������������� � FA  
  procedure add_fa_char_dttm(
    p_fa_id IN nasel_fa_char.fa_id%type,
    p_fa_char_type_cd in nasel_fa_char.char_type_cd%type,
    p_fa_char_val_dttm in nasel_fa_char.char_val_dttm%type
  );

     
  -- ������������� ���� ��������
  /*procedure set_fa_cc_dttm(
    p_fa_id  IN nasel_fa.fa_id%type,
    p_cc_dttm IN nasel_fa.cc_dttm%type
     );*/  
        
   
  -- ��������� ������� 
  function add_fa_cc(
    p_cc_dttm  in out nasel_cc.cc_dttm%type,
    p_cc_type_cd  IN nasel_cc.cc_type_cd%type,
    p_cc_status_flg in nasel_cc.cc_status_flg%type,
    p_acct_id  IN nasel_cc.acct_id%type,
    p_descr  IN nasel_cc.descr%type,
    p_src_id  IN nasel_cc.src_id%type,
    p_src_type_cd  in nasel_cc.src_type_cd%type,
    p_caller  IN nasel_cc.caller%type
  ) return nasel_cc.cc_id%type;

  procedure update_cc (  
    p_cc_id in nasel_cc.cc_id%type,
    p_cc_dttm  in out nasel_cc.cc_dttm%type,
    p_cc_type_cd  IN nasel_cc.cc_type_cd%type,
    p_cc_status_flg in nasel_cc.cc_status_flg%type,
    p_descr  IN nasel_cc.descr%type,
    p_caller  in nasel_cc.caller%type  
  );
  procedure delete_cc (p_cc_id in nasel_cc.cc_id%type);
  procedure delete_fp (p_fa_pack_id in nasel_fa_pack.fa_pack_id%type);
  procedure set_cc_status_flg(p_cc_id in nasel_cc.cc_id%type, p_cc_status_flg in nasel_cc.cc_status_flg%type);
  procedure set_cc_approval(p_cc_id in nasel_cc.cc_id%type);
  
  procedure add_fa_char_sa_e_dt;
  procedure create_fp_cancel_stop;
  
  procedure create_fp_cancel_stop_force(p_fa_id_list in t_fa_id_list);


  



  /* ���������, ������������ ����� ������ */  
  procedure get_app_config(p_app_path in varchar2, p_app_ver in varchar2, rc out sys_refcursor);
  procedure get_acct_otdelen_list(rc out sys_refcursor);
  procedure get_op_area_cd_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor);
  procedure get_fp_type_list(rc out sys_refcursor);
  
  procedure get_fp_inf(p_fa_pack_id in nasel_fa_pack.fa_pack_id%type, rc out sys_refcursor);
  procedure get_fa_cc_inf(p_cc_id in nasel_cc.cc_id%type, rc out sys_refcursor);
  
  procedure get_acct_full_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor);
  procedure get_pre_notices_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor);
  procedure get_pre_post_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor);
  procedure get_approval_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor);
  procedure get_pre_stop_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor); -- get_stop_list
  procedure get_fp_cancel_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor);
  procedure get_fp_reconnect_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor);

  procedure get_fp_notices_content(p_fa_pack_id in nasel_fa_pack.fa_pack_id%type, rc out sys_refcursor);
  procedure get_fp_stop_content(p_fa_pack_id in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor);
  procedure get_fp_stop_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, p_fa_id in nasel_fa.fa_id%type, p_acct_id in nasel_ccb_prem.acct_id%type, rc out sys_refcursor);
  procedure get_fp_notices_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, p_fa_id in nasel_fa.fa_id%type, p_acct_id in nasel_ccb_prem.acct_id%type, rc out sys_refcursor);
  procedure get_fp_cancel_stop_content(p_fa_pack_id in nasel_fa_pack.fa_pack_id%type, rc out sys_refcursor);
 
  procedure get_fa_reconnect_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor);
  procedure get_fp_reconnect_content(p_fa_pack_id in nasel_fa_pack.fa_pack_id%type, rc out sys_refcursor);


end pk_nasel_sweety;
/
create or replace package body pk_nasel_sweety is

  -- Private type declarations
  /*type <TypeName> is <Datatype>;
  
  -- Private constant declarations
  <ConstantName> constant <Datatype> := <Value>;

  -- Private variable declarations
  <VariableName> <Datatype>;

  -- Function and procedure implementations
  function <FunctionName>(<Parameter> <Datatype>) return <Datatype> is
    <LocalVariable> <Datatype>;
  begin
    <Statement>;
    return(<Result>);
  end;*/


/*function is_test(p_fa_pack_id in nasel_fa_pack.fa_pack_id%type)
  return nasel_fa_pack.fa_pack_id%type
is
  v_child_fa_id nasel_fa.fa_id%type;
begin
    select
      max(child_fa.fa_id)
    into
      v_child_fa_id
    from nasel_fa
    
    inner join (
      select 
        nasel_fa.fa_id
        , nasel_fa.prnt_fa_id
      from nasel_fa_pack
      inner join nasel_fa on nasel_fa.fa_pack_id = nasel_fa_pack.fa_pack_id and nasel_fa_pack.fa_pack_status_flg not in ('10','20')
    ) child_fa on child_fa.prnt_fa_id = nasel_fa.fa_id

    where nasel_fa.fa_pack_id = p_fa_pack_id;
    
    return v_child_fa_id;
end;*/
  


/* ��������� ��� ��������� ��� ������ � �������������
*/
-- ��������� �����
function add_fp(
  p_fa_pack_type_cd  IN nasel_fa_pack.fa_pack_type_cd%type,
  p_prnt_fa_pack_id  IN nasel_fa_pack.prnt_fa_pack_id%type,
  p_acct_otdelen in nasel_fa_pack.acct_otdelen%type
   )
  return nasel_fa_pack.fa_pack_id%type
is
begin
  return create_fp(p_fa_pack_type_cd, p_prnt_fa_pack_id, p_acct_otdelen);
end;

-- ��������� �����
function create_fp(
  p_fa_pack_type_cd  IN nasel_fa_pack.fa_pack_type_cd%type,
  p_prnt_fa_pack_id  IN nasel_fa_pack.prnt_fa_pack_id%type,
  p_acct_otdelen in nasel_fa_pack.acct_otdelen%type
   )
  return nasel_fa_pack.fa_pack_id%type
is
  v_fa_pack_id nasel_fa_pack.fa_pack_id%type;
begin
  select lpad(seq_nasel_fa_pack_id.nextval, 10, '0')
  into v_fa_pack_id
  from dual;
  
  insert into nasel_fa_pack (
    fa_pack_id
    , fa_pack_type_cd
    , prnt_fa_pack_id
    , acct_otdelen
    , fa_pack_status_flg
  )
  values (
    v_fa_pack_id
    , p_fa_pack_type_cd
    , p_prnt_fa_pack_id
    , p_acct_otdelen  
    , '10'
  ); 
  commit;
  return v_fa_pack_id; 
end create_fp;


-- ������������� ��������� ���������� ��� �������
/*procedure add_fa_pack_char_recipient(
  p_fa_pack_id in nasel_fa_pack_char.fa_pack_id%type,         -- ������
  p_rt_spr_cd in nasel_fa_pack_char.char_val_str%type,        -- ��� �����������
  p_rt_spr in nasel_fa_pack_char.char_val_str%type,           -- ������������ �����������
  p_rt_addr in nasel_fa_pack_char.char_val_str%type,          -- ����� �����������
  p_rt_post in nasel_fa_pack_char.char_val_str%type,          -- ��������� 
  p_rt_name in nasel_fa_pack_char.char_val_str%type           -- ���
)
is
begin
  insert into nasel_fa_pack_char (
    fa_pack_id
    , char_type_cd
    , char_val_str
    , effdt
  )
  values (
    p_fa_pack_id
    , 'RT-SPRCD'
    , p_rt_spr_cd
    , sysdate
  ); 
  insert into nasel_fa_pack_char (
    fa_pack_id
    , char_type_cd
    , char_val_str
    , effdt
  )
  values (
    p_fa_pack_id
    , 'RT-SPR'
    , p_rt_spr
    , sysdate
  );
   
  insert into nasel_fa_pack_char (
    fa_pack_id
    , char_type_cd
    , char_val_str
    , effdt
  )
  values (
    p_fa_pack_id
    , 'RT-ADDR'
    , p_rt_addr
    , sysdate
  );
   
  insert into nasel_fa_pack_char (
    fa_pack_id
    , char_type_cd
    , char_val_str
    , effdt
  )
  values (
    p_fa_pack_id
    , 'RT-POST'
    , p_rt_post
    , sysdate
  ); 
  insert into nasel_fa_pack_char (
    fa_pack_id
    , char_type_cd
    , char_val_str
    , effdt
  )
  values (
    p_fa_pack_id
    , 'RT-NAME'
    , p_rt_name
    , sysdate
  ); 
  commit;
end;*/

/*
procedure update_fa_pack_char_recipient(p_fa_pack_id in nasel_fa_pack_char.fa_pack_id%type)
is
  v_rt_spr_cd nasel_fa_pack_char.char_val_str%type;        -- ��� ���������� �����
  v_rt_spr nasel_fa_pack_char.char_val_str%type;           -- ������������ �����������
  v_rt_addr nasel_fa_pack_char.char_val_str%type;          -- ����� �����������
  v_rt_post nasel_fa_pack_char.char_val_str%type;          -- ��������� 
  v_rt_name nasel_fa_pack_char.char_val_str%type;           -- ���
begin
  select 
    case when nasel_ccb_prem.prnt_prem_type_cd is null then spr.spr_cd end
    --, nvl(nasel_ccb_spr_l.descr,first_acct.spr_descr)
    --, nasel_ccb_spr_l.address
    --, padeg_pack.Padeg_FIO(nasel_ccb_spr_l.official_post, '�') official_post
    --, padeg_pack.Padeg_FIO(nasel_ccb_spr_l.official_name, '�') official_name

    , case when nasel_ccb_prem.prnt_prem_type_cd is null 
      then nasel_ccb_spr_l.descr
        --nvl(nasel_ccb_spr_l.descr, first_acct.spr_descr) -- ��� �� ����. �������� �.�.
      else nasel_acct_otdelen_vw.descr_l_genitive || ' ' || '�� ��� "���������������"' end
      
    , case when nasel_ccb_prem.prnt_prem_type_cd is null then nasel_ccb_spr_l.address else nasel_acct_otdelen_vw.mailing_address end address
    , padeg_pack.Padeg_FIO(case when nasel_ccb_prem.prnt_prem_type_cd is null then nasel_ccb_spr_l.official_post else '���������' end, '�') official_post
    , padeg_pack.Padeg_FIO(case when nasel_ccb_prem.prnt_prem_type_cd is null then nasel_ccb_spr_l.official_name else nasel_acct_otdelen_vw.director end, '�') official_name

  into 
    v_rt_spr_cd
    , v_rt_spr 
    , v_rt_addr
    , v_rt_post
    , v_rt_name
  from nasel_fa_pack

  left join
  (
    select 
      nasel_fa.fa_pack_id  
      , nasel_fa.acct_id 
      --, spr.descr spr_descr  
      --, spr.spr_cd
      , row_number() over (partition by nasel_fa.fa_pack_id order by null) n  
      , count(nasel_fa.acct_id) over (partition by nasel_fa.fa_pack_id order by null) acct_id_cnt
    from nasel_fa
  ) first_acct on first_acct.fa_pack_id = nasel_fa_pack.fa_pack_id and first_acct.n = 1
  
  left join nasel_ccb_prem on nasel_ccb_prem.acct_id = first_acct.acct_id     
  
  left join (
    select        
      nasel_ccb_sa_rel.acct_id
      , nasel_ccb_spr.descr 
      , nasel_ccb_spr.spr_cd
      , row_number() over (partition by nasel_ccb_sa_rel.acct_id order by nasel_ccb_sa_rel.sa_rel_type_cd) n
    from nasel_ccb_sa_rel
    inner join nasel_ccb_spr on nasel_ccb_spr.spr_cd = nasel_ccb_sa_rel.spr_cd
  ) spr on spr.acct_id = first_acct.acct_id and spr.n = 1 and nasel_ccb_prem.prnt_prem_type_cd is null
  
  -- ���������� �� �������
  left join nasel_acct_otdelen_vw on nasel_acct_otdelen_vw.acct_otdelen = nasel_ccb_prem.acct_otdelen 
    --and nasel_ccb_prem.prem_type_cd not in ('CD', 'DCA')

  left join nasel_ccb_spr_l on nasel_ccb_spr_l.spr_cd = spr.spr_cd
  
  where nasel_fa_pack.fa_pack_id = p_fa_pack_id; 

  -- ��������� ��������������
  add_fa_pack_char_str(p_fa_pack_id, 'RT-SPRCD', v_rt_spr_cd);
  add_fa_pack_char_str(p_fa_pack_id, 'RT-SPR', v_rt_spr);
  add_fa_pack_char_str(p_fa_pack_id, 'RT-ADDR', v_rt_addr);
  add_fa_pack_char_str(p_fa_pack_id, 'RT-NAME', v_rt_post);
  add_fa_pack_char_str(p_fa_pack_id, 'RT-POST', v_rt_name);
  
  commit;

end;*/

/* ��������������� � ������������� ������ */
function get_fp_char_list (p_fa_pack_id in nasel_fa_pack_char.fa_pack_id%type)
  return t_char_list
  is
  char_list t_char_list;
  begin
    for r in (
      select trim(char_type_cd) char_type_cd, char_val_str
      from nasel_fa_pack_char 
      where fa_pack_id = p_fa_pack_id) loop
      
      char_list(trim(r.char_type_cd)) := r.char_val_str;
    end loop;
    
    /*select char_type_cd, char_val_str
    from nasel_fa_pack_char 
    where fa_pack_id = p_fa_pack_id;*/
    return char_list;
  end;
  
  /* ��������������� ����� �������*/
/*procedure get_fa_pack_char_list1 (p_fa_pack_id in nasel_fa_pack_char.fa_pack_id%type
  , rc out sys_refcursor)
  is
  --char_list t_char_list;
  begin
    
    open rc for 
      select max(case when char_type_cd = 'RT-ADDR' then char_val_str end) rt_addr
        , max(case when char_type_cd = 'RT-NAME' then char_val_str end) rt_name
        , max(case when char_type_cd = 'RT-POST' then char_val_str end) rt_post
        , max(case when char_type_cd = 'RT-SPR' then char_val_str end) rt_spr
        , max(case when char_type_cd = 'RT-SPRCD' then char_val_str end) rt_sprcd
        , max(case when char_type_cd = 'RT-TYPE' then char_val_str end) rt_type
      from nasel_fa_pack_char 
      where fa_pack_id = p_fa_pack_id
      group by fa_pack_id;
  end;*/
  
  
  /* ��������������� ��������, ���� ������� - ������� */
function get_rt_otdelen (p_acct_otdelen in nasel_acct_otdelen_vw.acct_otdelen%type)
  return t_char_list
  is
  char_list t_char_list;
  begin

    for r in (
      select *
      from nasel_acct_otdelen_vw 
      where nasel_acct_otdelen_vw.acct_otdelen = p_acct_otdelen) loop
      
      char_list('RT-SPR') := r.descr_l_genitive || ' �� ��� "���������������"';
      char_list('RT-POST') := '����������';
      char_list('RT-NAME') := padeg_pack.Padeg_FIO(r.director, '�') ;
      char_list('RT-ADDR') := r.Mailing_Address;
      --char_list('RT-SPR') := null;
    end loop;
    return char_list;
  end;
  
  /* ��������������� ��������, ���� ������� - ��������� ����� (��������� �����������) */
function get_rt_spr (p_spr_cd in nasel_ccb_spr.spr_cd%type)
  return t_char_list
  is
  char_list t_char_list;
  v_found boolean;
  begin

    for r in (
      select * 
      from nasel_ccb_spr_l
      where trim(nasel_ccb_spr_l.spr_cd) = p_spr_cd) loop
      
      v_found := true;
      char_list('RT-SPRCD') := r.spr_cd;
      char_list('RT-SPR') := r.descr;
      char_list('RT-POST') := padeg_pack.Padeg_FIO(r.official_post, '�');
      char_list('RT-NAME') := padeg_pack.Padeg_FIO(r.official_name, '�');
      char_list('RT-ADDR') := r.address;
    end loop;
    
    if nvl(v_found,false) <> true then
      char_list('RT-SPRCD') := null;
      char_list('RT-SPR') := null;
      char_list('RT-POST') := null;
      char_list('RT-NAME') := null;
      char_list('RT-ADDR') := null;
    end if;
    
    return char_list;
  end;  
  
  

/* ��������� �������������� ����������� ��� ������� */
procedure add_fp_char_recipient(p_fa_pack_id in nasel_fa_pack_char.fa_pack_id%type, p_force_self integer)
is
  v_rt_spr_cd nasel_fa_pack_char.char_val_str%type;        -- ��� ���������� �����
  v_rt_spr nasel_fa_pack_char.char_val_str%type;           -- ������������ �����������
  v_rt_addr nasel_fa_pack_char.char_val_str%type;          -- ����� �����������
  v_rt_post nasel_fa_pack_char.char_val_str%type;          -- ��������� 
  v_rt_name nasel_fa_pack_char.char_val_str%type;           -- ���
  v_rt_type nasel_fa_pack_char.char_val_str%type;           -- ��� �������� (OTDELEN/SPR)
  
  v_prnt_prem_type_cd nasel_ccb_prem.prnt_prem_type_cd%type;
begin
  -- ���� p_force_self = 1, �� ������ ������ ��� �������
  -- ���� p_force_self = 0, �� ������ ������ � ����������� �� ��.�������  

  
  v_rt_spr_cd := null;
  if p_force_self = 0 then -- ���� �� ������ �������������� ������� - �������
    select 
      nasel_ccb_prem.prnt_prem_type_cd
    into
      v_prnt_prem_type_cd
    
    from nasel_fa_pack

    left join
    (
      select 
        nasel_fa.fa_pack_id  
        , nasel_fa.acct_id 
        , row_number() over (partition by nasel_fa.fa_pack_id order by null) n  
      from nasel_fa
    ) first_acct on first_acct.fa_pack_id = nasel_fa_pack.fa_pack_id and first_acct.n = 1
    left join nasel_ccb_prem on nasel_ccb_prem.acct_id = first_acct.acct_id     
    
    where nasel_fa_pack.fa_pack_id = p_fa_pack_id;
  end if;
  
  
  -- ��������, ���� ������ ������
  if p_force_self = 1 or v_prnt_prem_type_cd is not null then
    -- ���� ������ ������������� ��� ��� ������������� �� �� NULL
    -- ������ ������ ���� (���������� �������)

    v_rt_type := 'OTDELEN';

    select  
      --nasel_acct_otdelen_vw.descr_l_genitive || ' ' || '�� ��� "���������������"' descr_l
      nasel_acct_otdelen_vw.descr_l_genitive || ' ' || '�� ��� "���������������"' descr_l
      , nasel_acct_otdelen_vw.mailing_address address
      , padeg_pack.Padeg_FIO('���������', '�') official_post
      , padeg_pack.Padeg_FIO(nasel_acct_otdelen_vw.director, '�') official_name
    into
      v_rt_spr 
      , v_rt_addr
      , v_rt_post
      , v_rt_name
    from nasel_fa_pack 
    left join nasel_acct_otdelen_vw on nasel_acct_otdelen_vw.acct_otdelen = nasel_fa_pack.acct_otdelen

    where nasel_fa_pack.fa_pack_id = p_fa_pack_id; 
  
  else
    -- ����� ������ ������ ��� ���������� �����
    v_rt_type := 'SPR';
    
    select 
      spr.spr_cd
      , nasel_ccb_spr_l.descr
      , nasel_ccb_spr_l.address 
      , padeg_pack.Padeg_FIO(nasel_ccb_spr_l.official_post, '�') official_post
      , padeg_pack.Padeg_FIO(nasel_ccb_spr_l.official_name, '�') official_name
    into 
      v_rt_spr_cd
      , v_rt_spr 
      , v_rt_addr
      , v_rt_post
      , v_rt_name
    from nasel_fa_pack

    left join
    (
      select 
        nasel_fa.fa_pack_id  
        , nasel_fa.acct_id 
        --, spr.descr spr_descr  
        --, spr.spr_cd
        , row_number() over (partition by nasel_fa.fa_pack_id order by null) n  
        , count(nasel_fa.acct_id) over (partition by nasel_fa.fa_pack_id order by null) acct_id_cnt
      from nasel_fa
    ) first_acct on first_acct.fa_pack_id = nasel_fa_pack.fa_pack_id and first_acct.n = 1
    
    left join nasel_ccb_prem on nasel_ccb_prem.acct_id = first_acct.acct_id     
    
    left join (
      select        
        nasel_ccb_sa_rel.acct_id
        , nasel_ccb_spr.descr 
        , nasel_ccb_spr.spr_cd
        , row_number() over (partition by nasel_ccb_sa_rel.acct_id order by nasel_ccb_sa_rel.sa_rel_type_cd) n
      from nasel_ccb_sa_rel
      inner join nasel_ccb_spr on nasel_ccb_spr.spr_cd = nasel_ccb_sa_rel.spr_cd
    ) spr on spr.acct_id = first_acct.acct_id and spr.n = 1 and nasel_ccb_prem.prnt_prem_type_cd is null
    
    -- ���������� �� �������
    left join nasel_ccb_spr_l on nasel_ccb_spr_l.spr_cd = spr.spr_cd
    
    where nasel_fa_pack.fa_pack_id = p_fa_pack_id;     
  
  end if;
  -- ��������� ��������������
  add_fp_char_str(p_fa_pack_id, 'RT-TYPE', v_rt_type);
  add_fp_char_str(p_fa_pack_id, 'RT-SPRCD', v_rt_spr_cd);
  add_fp_char_str(p_fa_pack_id, 'RT-SPR', v_rt_spr);
  add_fp_char_str(p_fa_pack_id, 'RT-ADDR', v_rt_addr);
  add_fp_char_str(p_fa_pack_id, 'RT-NAME', v_rt_name);
  add_fp_char_str(p_fa_pack_id, 'RT-POST', v_rt_post);
  
  commit;
end;

-- ������������� ���� ��������� ��� ������� fa_pack
procedure set_fp_status_flg(
  p_fa_pack_id in nasel_fa_pack_char.fa_pack_id%type,
  p_fa_pack_status_flg in nasel_fa_pack.fa_pack_status_flg%type
)
is
  update_failed exception; 
begin
  update nasel_fa_pack 
  set nasel_fa_pack.fa_pack_status_flg = p_fa_pack_status_flg
  where nasel_fa_pack.fa_pack_id = p_fa_pack_id;
 
  if (sql%rowcount > 0) then 
    commit;
  else 
    raise update_failed;
  end if;  
  
  exception 
    when update_failed then
      rollback; 
      raise_application_error(-20000, '�� ������� ����� ������ ID ' || p_fa_pack_id || '.'); 
  /*exception
   when NO_DATA_FOUND then
    raise_application_error(-20000, '�� ������� ����� ������� � ID ' || p_fa_pack_id || '.'); 
      dbms_output.put_line('Table name not found in query ');*/
end;

-- ���������� (���������) ������
procedure set_fp_status_flg_frozen(
  p_fa_pack_id in nasel_fa_pack.fa_pack_id%type
)
is
  v_fa_count integer;
begin
  select count(*)
  into v_fa_count
  from nasel_fa
  where nasel_fa.fa_pack_id = p_fa_pack_id; 
  
  if v_fa_count <> 0 then
    -- ����� ������� �������� �� ��� �������
    --update_fa_pack_char_recipient(p_fa_pack_id); -- ��������� �������������� �������� ��� ������� �� ����������� - �������� � ���������� �����
    
    set_fp_status_flg(p_fa_pack_id, '50'); 
  else
    raise_application_error(-20000, '������ ��������� ������ � ID ' || p_fa_pack_id || ' ��� ��� �� ������.'); 
  end if;
end;

-- ��������� ������ � ������ �� ��������
/*procedure set_fa_pack_status_flg_incomp(
  p_fa_pack_id in nasel_fa_pack.fa_pack_id%type
)
is
  --v_fa_count integer;
begin
  set_fa_pack_status_flg(p_fa_pack_id, '10'); 
end;*/

-- ��������� ������ � ������ �������
procedure set_fp_status_flg_cancel(
  p_fa_pack_id in nasel_fa_pack.fa_pack_id%type
)
is
  v_child_fa_id nasel_fa.fa_id%type;
begin

  -- ������ ������ ������������ ��������, ���� �� ���������� ������� � ������� ������������� ���������
  select
    max(child_fa.fa_id)
  into
    v_child_fa_id
  from nasel_fa
    
  inner join (
    select 
      nasel_fa.fa_id
      , nasel_fa.prnt_fa_id
    from nasel_fa_pack
    inner join nasel_fa on nasel_fa.fa_pack_id = nasel_fa_pack.fa_pack_id and nasel_fa_pack.fa_pack_status_flg not in ('10','20')
  ) child_fa on child_fa.prnt_fa_id = nasel_fa.fa_id

  where nasel_fa.fa_pack_id = p_fa_pack_id;
 -- v_child_fa_id := pk_nasel_sweety.is_test(:old.fa_pack_id);
    
  if v_child_fa_id is not null then
    raise_application_error(-20003, '�� ���������� � ������ ������� ������� ������ � ������ �������� �������� (��. ' || v_child_fa_id || ').' ); 
  end if;  

  
  set_fp_status_flg(p_fa_pack_id, '20'); 
end;


-- ������������� ������ ������� [��������� �����������]
procedure set_fp_stat_sent_perf(
  p_fa_pack_id in nasel_fa_pack.fa_pack_id%type
)
is
begin
  set_fp_status_flg(p_fa_pack_id, '71'); 
end;

-- ��������� ��������� �������������� ��� �������
procedure add_fp_char_str(
  p_fa_pack_id IN nasel_fa_pack_char.fa_pack_id%type,
  p_fa_pack_char_type_cd in nasel_fa_pack_char.char_type_cd%type,
  p_fa_pack_char_val_str in nasel_fa_pack_char.char_val_str%type
)
is
begin
  if p_fa_pack_char_val_str is not null then
    insert into nasel_fa_pack_char (
      fa_pack_id
      , char_type_cd
      , char_val_str
    )
    values (
      p_fa_pack_id
      , p_fa_pack_char_type_cd
      , p_fa_pack_char_val_str
    ); 
  end if;
end add_fp_char_str;

-- ��������� ���� �������������� ��� �������
procedure add_fp_char_dttm(
  p_fa_pack_id IN nasel_fa_pack_char.fa_pack_id%type,
  p_fa_pack_char_type_cd in nasel_fa_pack_char.char_type_cd%type,
  p_fa_pack_char_val_dttm in nasel_fa_pack_char.char_val_dttm%type
)
is
begin
  if p_fa_pack_char_val_dttm is not null then
    insert into nasel_fa_pack_char (
      fa_pack_id
      , char_type_cd
      , char_val_dttm
    )
    values (
      p_fa_pack_id
      , p_fa_pack_char_type_cd
      , p_fa_pack_char_val_dttm
    ); 
  end if;
end add_fp_char_dttm;

--
procedure add_fa_char_str(
  p_fa_id IN nasel_fa_char.fa_id%type,
  p_fa_char_type_cd in nasel_fa_char.char_type_cd%type,
  p_fa_char_val_str in nasel_fa_char.char_val_str%type
)
is
begin
  insert into nasel_fa_char (
    fa_id
    , char_type_cd
    , char_val_str
  )
  values (
    p_fa_id
    , p_fa_char_type_cd
    , p_fa_char_val_str
  ); 
end add_fa_char_str;

--
procedure add_fa_char_dttm(
  p_fa_id IN nasel_fa_char.fa_id%type,
  p_fa_char_type_cd in nasel_fa_char.char_type_cd%type,
  p_fa_char_val_dttm in nasel_fa_char.char_val_dttm%type
)
is
begin
  insert into nasel_fa_char (
    fa_id
    , char_type_cd
    , char_val_dttm
  )
  values (
    p_fa_id
    , p_fa_char_type_cd
    , p_fa_char_val_dttm
  ); 
end add_fa_char_dttm;



-- ��������� ������� ���� � �����
-- ��� ������� �������!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                               
/*procedure add_fa_to_pack_stop (
  p_acct_id  IN nasel_fa.acct_id%type,
  p_fa_pack_id  IN nasel_fa.fa_pack_id%type
   )
is
  --v_pack_cre_dttm nasel_fa_pack.cre_dttm%type;
  v_st_p_dt nasel_fa_char.char_val_dttm%type;
  v_fa_id number;
begin
  add_fa(p_acct_id, p_fa_pack_id);
  
  -- �������� ����� ���������� field Activity 
  --v_fa_id := SEQ_NASEL_FA_ID.currval;
  select SEQ_NASEL_FA_ID.currval
  into v_fa_id
  from dual;

  -- ��������� �������� ���� ����� �����������
  select greatest (
    (
      select nasel_fa_pack.cre_dttm + 10
      from nasel_fa_pack
      where nasel_fa_pack.fa_pack_id = p_fa_pack_id
    ),
    (
      select max(cc_dttm) + 30 
      from nasel_cc 
      where acct_id = p_acct_id and approval_dttm is not null
    )
  )
  into v_st_p_dt
  from dual; 
  
  -- ��������� �������� ���� � ��������� ������
  add_fa_char_dttm(lpad(v_fa_id, 10, '0'), 'ST-P-DT', trunc(v_st_p_dt,'dd'));

end add_fa_to_pack_stop;*/

-- ��������� ������������� 'ST-P-DT' (�������� ���� ����� �����������) 
-- � ������� ������������� nasel_fa_char
procedure set_fa_char_st_p_dt (
  p_fa_id nasel_fa.fa_id%type
)
is
  v_st_p_dt nasel_fa_char.char_val_dttm%type;
begin
  -- ��������� �������� ���� ����� �����������
  select 
    trunc(
      greatest(
        nvl(cc.cc_dttm + 30, to_date(1721424,'J'))
        , nasel_fa_pack.cre_dttm + 13
      ), 'dd')
  into v_st_p_dt
  from nasel_fa
  left join nasel_fa_pack on nasel_fa_pack.fa_pack_id = nasel_fa.fa_pack_id
  left join (
    select 
      nasel_cc.acct_id
      , max(nasel_cc.cc_dttm) cc_dttm
    from nasel_cc 
    where approval_dttm is not null
    group by nasel_cc.acct_id
  ) cc on cc.acct_id = nasel_fa.acct_id 
  where nasel_fa.fa_id = p_fa_id;--'0000009922'
  
  -- ��������� ������ ������� ����, ���� ���� ������ �� ��������
  select
    min(nasel_cal_work.day_dt)
  into v_st_p_dt
  from nasel_cal_work 
  where day_type_cd <> 'H' and day_dt >= v_st_p_dt;
  
  -- ��������� �������� ���� � ��������� ������
  add_fa_char_dttm(p_fa_id, 'ST-P-DT', v_st_p_dt);
end;




-- ������������� ���� ��������� ��� ������������ fa
-- �������� � ���������� �������, ��� ��� �� ��������� ��� ������ � ���������, ��������������� ����� ������
-- �� �������� ������� ��� �������� ������ ��� ������ � �������������
/*procedure set_fa_status_flg(
  p_fa_id IN nasel_fa.fa_id%type
  , p_fa_status_flg in nasel_fa.fa_status_flg%type
)
is
  update_failed exception; 
begin
  update nasel_fa 
  set nasel_fa.fa_status_flg = p_fa_status_flg
  where nasel_fa.fa_id = p_fa_id;
 
  if (sql%rowcount > 0) then 
    commit;
  else 
    raise update_failed;
  end if;  
  
  exception 
    when update_failed then
      rollback; 
      raise_application_error(-20000, '�� ������� ����� �� ID ' || p_fa_id || '.'); 
  /*exception
   when NO_DATA_FOUND then
    raise_application_error(-20000, '�� ������� ����� ������� � ID ' || p_fa_pack_id || '.'); 
      dbms_output.put_line('Table name not found in query ');*/
/*end;*/

-- ������� ������������ �� �������� ����� � ��������� �� � ������ (������ �������)
function create_fa_stg (
  p_acct_id  in nasel_fa.acct_id%type,
  p_fa_pack_id in nasel_fa.fa_pack_id%type,
  p_saldo_uch in nasel_fa.saldo_uch%type,
  p_mtr_serial_nbr in nasel_fa.mtr_serial_nbr%type,
  p_end_reg_reading1 in nasel_fa.end_reg_reading1%type,
  p_end_reg_reading2 in nasel_fa.end_reg_reading2%type
  --, p_fa_status_flg in nasel_fa.fa_status_flg%type
) return nasel_fa.fa_id%type 
is
  v_fa_id nasel_fa.fa_id%type;
begin
  insert into nasel_fa (
    acct_id
    , fa_pack_id
    , saldo_uch
    , mtr_serial_nbr
    , end_reg_reading1
    , end_reg_reading2
    --, fa_status_flg
  )
  values (
    p_acct_id
    , p_fa_pack_id
    , p_saldo_uch
    , p_mtr_serial_nbr
    , p_end_reg_reading1
    , p_end_reg_reading2
    --, p_fa_status_flg
  ) 
  returning fa_id into v_fa_id;
  commit;
  
  DBMS_OUTPUT.put_line('FA ' || to_char(v_fa_id) || 
                           ' (acct_id: ' || to_char(p_acct_id) || ') added to PACK: ' || to_char(p_fa_pack_id));
  
  return v_fa_id;
end create_fa_stg;


/* ������� ������ - ������� ��� �������� ������*/
procedure create_fp_stg (acct_otdelen nasel_fa_pack.acct_otdelen%type)
is
  v_fa_pack_id nasel_fa_pack.fa_pack_id%type;
  v_fa_id nasel_fa.fa_id%type;
  
  cursor p_cursor is 
  select * from joiner_stgup;
  
  v_res p_cursor%rowtype;
  
begin
  
  v_fa_pack_id := pk_nasel_sweety.create_fp('10',null, acct_otdelen);
  
  --v_fa_pack_id := '0000002222';

  for v_res in p_cursor
  loop
    v_fa_id := pk_nasel_sweety.create_fa_stg(
      lpad(v_res.F1,10,'0')      -- acct_id
      , v_fa_pack_id                        -- fa_pack_id
      , to_number(v_res.F2, '999999999.99') -- saldo_uch
      , ''                                  -- mtr_serial_nbr
      , to_number(v_res.F3, '999999999.99') -- end_reg_reading1
      , to_number(v_res.F4, '999999999.99') -- end_reg_reading2
    );
  end loop;

  pk_nasel_sweety.set_fp_status_flg_frozen(v_fa_pack_id);

end create_fp_stg;


-- ������� ������������ �� �������� ����� � ��������� �� � ������
function create_fa (
  p_acct_id  in nasel_fa.acct_id%type,
  p_fa_pack_id in nasel_fa.fa_pack_id%type,
  p_prnt_fa_id in nasel_fa.prnt_fa_id%type
) return nasel_fa.fa_id%type 
is
  v_fa_id nasel_fa.fa_id%type; 
  insert_failed exception; 
  --v_fa_pack_status_flg nasel_fa_pack.fa_pack_status_flg%type;  
  --fa_pack_closed exception;   
  --fa_pack_not_found exception;
begin
  /*insert into nasel_fa (acct_id, fa_pack_id)
  values (p_acct_id, p_fa_pack_id); */   
                     
  /*greatest(1,2)
  select p_fa_pack_id
  into
  from nasel_fa_pack;  */
  
  /*select fa_pack_status_flg
  into v_fa_pack_status_flg
  from nasel_fa_pack
  where nasel_fa_pack.fa_pack_id = p_fa_pack_id;
  
  if (v_fa_pack_status_flg = '50') then -- ������ ��������� � ������ � ������� ���������
    raise fa_pack_closed;
  end if;*/
  
  insert into nasel_fa (
    acct_id
    , fa_pack_id
    , prnt_fa_id
    , saldo_uch
    , mtr_serial_nbr
    , end_reg_reading1
    , end_reg_reading2
  )
  select 
    p_acct_id
    , p_fa_pack_id 
    , p_prnt_fa_id
    , nvl(nasel_ccb_ft.saldo_bt_uch, 0) + nvl(nasel_ccb_ft.saldo_odn_uch, 0) + nvl(nasel_ccb_ft.saldo_act_uch, 0) + nvl(nasel_ccb_ft.saldo_pen_uch, 0)  saldo_uch
    , nasel_ccb_sp.mtr_serial_nbr
    , nasel_ccb_bseg_read.end_reg_reading1
    , nasel_ccb_bseg_read.end_reg_reading2
  from nasel_ccb_ft
  left join nasel_ccb_bseg_read on nasel_ccb_bseg_read.acct_id = nasel_ccb_ft.acct_id
  left join nasel_ccb_sp on nasel_ccb_sp.acct_id = nasel_ccb_ft.acct_id
  where nasel_ccb_ft.acct_id = p_acct_id;
    
      
  if (sql%rowcount > 0) then 
    commit;
    select lpad(SEQ_NASEL_FA_ID.currval, 10, '0')
    into v_fa_id
    from dual;
  else 
    raise insert_failed;
  end if;  
  
  return v_fa_id;

  exception 
    when insert_failed then
      raise_application_error(-20000, '���������� � ������ �� �����������.'); 
  
  

 /* if (sql%rowcount > 0) then 
    commit;
    return v_fa_id;
  else 
    raise fa_pack_not_found;
  end if;  */

  /*exception 
    when fa_pack_closed then
      rollback; 
      raise_application_error(-20001, '������ � ID ' || p_fa_pack_id || ' ��������� � ������� ''���������''. ���������� � ���� �������������� �������� ���������.'); 
    when fa_pack_not_found then
      rollback; 
      raise_application_error(-20000, '�� ������� ����� ������� � ID ' || p_fa_pack_id || '.'); 
 */ 
end create_fa;



-- ������� ������������ �� �������� ����� � ��������� �� � ������
function create_fa_stop (
  p_acct_id  in nasel_fa.acct_id%type,
  p_fa_pack_id in nasel_fa.fa_pack_id%type
) return nasel_fa.fa_id%type 
is
  v_fa_id nasel_fa.fa_id%type;
  --v_fa_notice nasel_fa.fa_id%type; 
  insert_failed exception; 
begin
  insert into nasel_fa (
    acct_id
    , fa_pack_id
    , saldo_uch
    , mtr_serial_nbr
    , end_reg_reading1
    , end_reg_reading2
    , prnt_fa_id       -- ����� �����������
  )
  select 
    p_acct_id
    , p_fa_pack_id
    , nvl(nasel_ccb_ft.saldo_bt_uch, 0) + nvl(nasel_ccb_ft.saldo_odn_uch, 0) + nvl(nasel_ccb_ft.saldo_act_uch, 0) + nvl(nasel_ccb_ft.saldo_pen_uch, 0)  saldo_uch
    , nasel_ccb_sp.mtr_serial_nbr
    , nasel_ccb_bseg_read.end_reg_reading1
    , nasel_ccb_bseg_read.end_reg_reading2
    , cc.fa_id prtn_fa_id
  from nasel_ccb_ft
  left join nasel_ccb_bseg_read on nasel_ccb_bseg_read.acct_id = nasel_ccb_ft.acct_id
  left join nasel_ccb_sp on nasel_ccb_sp.acct_id = nasel_ccb_ft.acct_id
  
  -- ��������� �� ���� �������� �����������
  inner join (
    select
      nasel_fa.fa_id
      , nasel_cc.acct_id
      , row_number() over (partition by nasel_fa.acct_id order by nasel_fa_pack.cre_dttm desc) N
    from nasel_fa_pack
    inner join nasel_fa on nasel_fa.fa_pack_id = nasel_fa_pack.fa_pack_id
    inner join nasel_cc on nasel_cc.src_id = nasel_fa.fa_id 
      and nasel_cc.src_type_cd = '10' -- ������� �� �����������
      and nasel_cc.cc_status_flg in ('20') -- ����������� �������
      and nasel_cc.approval_dttm is not null
    
    where nasel_fa_pack.fa_pack_type_cd in ('10','20') -- ��������� � �����
      and nasel_fa_pack.fa_pack_status_flg in  ('50')      -- ������������
  ) cc on cc.acct_id = nasel_ccb_ft.acct_id and cc.n = 1
  where nasel_ccb_ft.acct_id = p_acct_id;
  
  if (sql%rowcount > 0) then 
    commit;
    select lpad(SEQ_NASEL_FA_ID.currval, 10, '0')
    into v_fa_id
    from dual;
  else 
    raise insert_failed;
  end if;  
  
  -- ��������� �������������� ST-P-DT - �������� ���� ����������
  if v_user_app_ver_dt < to_date('20170627', 'yyyymmddhh24miss') then -- ������� ��� ����������
     set_fa_char_st_p_dt(v_fa_id);
     commit;
  end if; 
  
  return v_fa_id;

  exception 
    when insert_failed then
      raise_application_error(-20000, '���������� � ������ �� �����������.'); 
  
end create_fa_stop;



/*procedure set_fa_cc_dttm(
  p_fa_id  IN nasel_fa.fa_id%type,
  p_cc_dttm IN nasel_fa.cc_dttm%type
   )
is
begin
  update nasel_fa set nasel_fa.cc_dttm = p_cc_dttm
  where nasel_fa.fa_id = p_fa_id;
  commit;
end;*/


-- ������ ������ �������� � ���������
procedure set_cc_status_flg( 
  p_cc_id in nasel_cc.cc_id%type,
  p_cc_status_flg in nasel_cc.cc_status_flg%type
)
is
begin
  update nasel_cc set nasel_cc.cc_status_flg = p_cc_status_flg
  where nasel_cc.cc_id = p_cc_id;
  commit;
end;


-- ������ ���� �����������
-- ���, �����, ���������� �������.
procedure set_cc_approval( 
  p_cc_id in nasel_cc.cc_id%type
)
is
begin
  update nasel_cc set nasel_cc.approval_dttm = sysdate
--    , nasel_cc.approver_user_id = substr(sys_context('USERENV','SESSION_USER'),1,10)
  where nasel_cc.cc_id = p_cc_id
    and trim(nasel_cc.cc_status_flg) = '20' -- ���� ������� ������� -- ��� �������� � ���������� ��������� � �������
    and nasel_cc.approval_dttm is null;    -- ���� ������� ��� �� ���������
  commit;
end;

-- ������� �������
procedure delete_cc (p_cc_id in nasel_cc.cc_id%type)
is
  v_cc_id nasel_cc.cc_id%type;
  v_approval_dttm nasel_cc.approval_dttm%type;
begin
  select nasel_cc.cc_id, nasel_cc.approval_dttm
  into v_cc_id, v_approval_dttm  
  from nasel_cc 
  where nasel_cc.cc_id = p_cc_id; 
  
  if v_approval_dttm is not null then
    raise_application_error(-20000, '������� � ID ' || p_cc_id || ' ��� ���������. �� �� ������ ��� �������.'); 
  else
    delete from nasel_cc where nasel_cc.cc_id = p_cc_id;
    commit; 
  end if;
end;

-- ������ ��������� � ��������� ��������
procedure update_cc (  
  p_cc_id in nasel_cc.cc_id%type,
  p_cc_dttm in out nasel_cc.cc_dttm%type,
  p_cc_type_cd  IN nasel_cc.cc_type_cd%type,
  p_cc_status_flg in nasel_cc.cc_status_flg%type,
  p_descr  IN nasel_cc.descr%type,
  p_caller  in nasel_cc.caller%type  
) 
is
  v_cc_id nasel_cc.cc_id%type;
  v_approval_dttm nasel_cc.approval_dttm%type;
begin
  select nasel_cc.cc_id
    , nasel_cc.approval_dttm
  into v_cc_id
    , v_approval_dttm  
  from nasel_cc 
  where nasel_cc.cc_id = p_cc_id; 
  
  if v_approval_dttm is not null then
    raise_application_error(-20000, '������� � ID ' || p_cc_id || ' ��� ���������. �� �� ������ ��� ��������.'); 
  end if;
  
  -- ���� ���� ����� ��������� �������, ����� ��������� ���������� 
  if v_cc_id is not null then
    update nasel_cc 
    set nasel_cc.cc_dttm = p_cc_dttm 
      , nasel_cc.cc_type_cd = p_cc_type_cd
      , nasel_cc.cc_status_flg = p_cc_status_flg 
      , nasel_cc.descr = p_descr
      , nasel_cc.caller = p_caller 
    where nasel_cc.cc_id = p_cc_id; 
    commit;  
  else
    raise_application_error(-20001, '������� � ID ' || p_cc_id || ' �� ������'); 
  end if;
end;

-- ��������� ������� � ���������
function add_fa_cc (
  p_cc_dttm  in out nasel_cc.cc_dttm%type,
  p_cc_type_cd  IN nasel_cc.cc_type_cd%type,
  p_cc_status_flg in nasel_cc.cc_status_flg%type,
  p_acct_id  IN nasel_cc.acct_id%type,
  p_descr  IN nasel_cc.descr%type,
  p_src_id  IN nasel_cc.src_id%type,
  p_src_type_cd  IN nasel_cc.src_type_cd%type,
  p_caller  in nasel_cc.caller%type
) return nasel_cc.cc_id%type  
is
  cc_count integer;
  v_cc_id nasel_cc.cc_id%type; 
  v_fa_pack_cre_dttm nasel_fa_pack.cre_dttm%type;
begin
 
  -- ���� ��� ��������� - ������ ���������, ����� ���� ����� �������� �������
  -- �������� ���-�� (������� �������) ��������� �� ����������� � ���� �������� �����������
  if p_src_type_cd = '10' then
    select 
      count(nasel_cc.cc_id)
      , max(nasel_fa_pack.cre_dttm) 
    into cc_count
      , v_fa_pack_cre_dttm 
    from nasel_fa
    inner join nasel_fa_pack on nasel_fa_pack.fa_pack_id = nasel_fa.fa_pack_id
    left join nasel_cc on nasel_cc.src_id = nasel_fa.fa_id and nasel_cc.src_type_cd = '10'
    where 
       nasel_fa.fa_id = p_src_id;  
  else
    raise_application_error(-20000, '�������������� ��� ��������� (�����������).' ); 
  end if;      
  
  -- �������� ������� ���� �������� �� ����� ���� ����� ���� �������� �����������.
  if p_cc_dttm < trunc(v_fa_pack_cre_dttm) then
    raise_application_error(-20001, '���� �������� �� ����� ���� ����� ���� �������� �����������.' ); 
  end if;
  
  if nvl(cc_count, 0) = 0 then 
    -- ������� ����� �������
    insert into nasel_cc (
      cc_dttm, cc_type_cd, cc_status_flg, acct_id, descr, src_id, src_type_cd, caller
    )
    values (
      p_cc_dttm, p_cc_type_cd, p_cc_status_flg, p_acct_id, p_descr, p_src_id, p_src_type_cd, p_caller
    ) returning cc_id into v_cc_id; 
    commit;
  else 
    -- raise_application_error(-20000, '����������� ������� ����� ' || p_src_id || ' - ' || cc_count || '  < ' ); 
    null; -- ������?
  end if;   
    
  return v_cc_id;
end; 


-- ������� �������
procedure delete_fp (p_fa_pack_id in nasel_fa_pack.fa_pack_id%type)
is
begin
  delete from nasel_fa_pack where nasel_fa_pack.fa_pack_id = p_fa_pack_id;
  commit;
  /*select nasel_cc.cc_id, nasel_cc.approval_dttm
  into v_cc_id, v_approval_dttm  
  from nasel_cc 
  where nasel_cc.cc_id = p_cc_id; 
  
  if v_approval_dttm is not null then
    raise_application_error(-20000, '������� � ID ' || p_cc_id || ' ��� ���������. �� �� ������ ��� �������.'); 
  else
    delete from nasel_cc where nasel_cc.cc_id = p_cc_id;
    commit; 
  end if;*/
end;


-- ��������� ��� ��������������� ����������
-- ����������� �������������� SA-E-DT (���� ���������� ���) ��� ��������� �� ����������
-- �����! �������� ������� ���������, ���� �� ������ �� �������������.
-- �������� ������� ����� ������ ��������� ������ �� ����������. �������.
procedure add_fa_char_sa_e_dt
  is
begin
  insert 
  into nasel_fa_char (
    nasel_fa_char.fa_id
    , nasel_fa_char.char_type_cd
    , nasel_fa_char.char_val_dttm
    , nasel_fa_char.effdt
  )
  select fa.fa_id
    , 'SA-E-DT' 
    , nasel_ccb_sp.sa_end_dt 
    , sysdate
  from nasel_ccb_sp
  
  -- ��������� �� �� ������ �� ����������
  inner join (
    select 
      nasel_fa.acct_id
      , nasel_fa.fa_id
      , row_number() over (partition by nasel_fa.acct_id order by nasel_fa_pack.cre_dttm) n
    from nasel_fa_pack
    inner join nasel_fa on nasel_fa.fa_pack_id = nasel_fa_pack.fa_pack_id
    where nasel_fa_pack.fa_pack_type_cd = '40' and nasel_fa_pack.fa_pack_status_flg = '50'
  ) fa on fa.acct_id = nasel_ccb_sp.acct_id and fa.n = 1
 
  left join (
    select
      nasel_fa_char.fa_id
      , nasel_fa_char.char_val_dttm sa_e_dt
    from nasel_fa_char
    where nasel_fa_char.char_type_cd = 'SA-E-DT'
  ) fa_char on fa_char.fa_id = fa.fa_id
  
  where nasel_ccb_sp.sa_status_flg in ('40','60') and nasel_ccb_sp.sa_end_dt is not null and fa_char.sa_e_dt is null;

  commit;  
  /*select nasel_fa.fa_id
    , 'SA-E-DT' 
    , nasel_ccb_sp.sa_end_dt 
    , sysdate
  from nasel_fa_pack
  inner join nasel_fa on nasel_fa.fa_pack_id = nasel_fa_pack.fa_pack_id
  left join (
    select
      nasel_fa_char.fa_id
      , nasel_fa_char.char_val_dttm sa_e_dt
    from nasel_fa_char
    where nasel_fa_char.char_type_cd = 'SA-E-DT'
  ) fa_char on fa_char.fa_id = nasel_fa.fa_id
  inner join nasel_ccb_sp on nasel_ccb_sp.acct_id = nasel_fa.acct_id and nasel_ccb_sp.sa_end_dt is not null 
  where nasel_fa_pack.fa_pack_type_cd = '40' and nasel_fa_pack.fa_pack_status_flg = '50'
    and fa_char.sa_e_dt is null;*/
end;



--
--
--
procedure create_fp_stop(p_acct_id_list in t_acct_id_list, p_force_self number)
  is
  cursor p_cursor is   
    select nasel_ccb_prem.acct_id 
    , nasel_ccb_prem.prem_type_cd
    , nasel_ccb_prem.acct_otdelen
    --, nasel_ccb_sa_rel.spr_cd
    --, nvl2(nasel_ccb_sa_rel.spr_cd, '1', nasel_ccb_prem.prem_type_cd, '2', nasel_ccb_upr.kod_upr, '3')
    
    , case when p_force_self = 0 
      then nvl2(nasel_ccb_sa_rel.spr_cd, 'SPR', nvl2(nasel_ccb_upr.kod_upr, 'UPR', 'OTDELEN')) 
      else 'OTDELEN'
      end rt_type
      
    , case when p_force_self = 0 then 
        dense_rank() over (order by nasel_ccb_prem.acct_otdelen, nasel_ccb_sa_rel.spr_cd, nasel_ccb_upr.kod_upr)
      else 1 end rnk
    
    , case when p_force_self = 0 
      then nvl2(nasel_ccb_sa_rel.spr_cd, nasel_ccb_spr_l.spr_cd, nvl2(nasel_ccb_upr.kod_upr, nasel_ccb_upr.kod_upr, nasel_acct_otdelen_vw.acct_otdelen))
      else nasel_acct_otdelen_vw.acct_otdelen 
      end rt_spr
      
    , case when p_force_self = 0 
      then nvl2(nasel_ccb_sa_rel.spr_cd, nasel_ccb_spr_l.descr, nvl2(nasel_ccb_upr.kod_upr, nasel_ccb_upr.upr_descr, nasel_acct_otdelen_vw.descr_l || ' �� ��� "���������������"' )) 
      else nasel_acct_otdelen_vw.descr_l || ' �� ��� "���������������"'
      end rt_spr_cd
      
    , case when p_force_self = 0 
      then nvl2(nasel_ccb_sa_rel.spr_cd, nasel_ccb_spr_l.official_post, nvl2(nasel_ccb_upr.kod_upr, nasel_ccb_upr.director_job_descr, nasel_acct_otdelen_vw.acting_post)) 
      else nasel_acct_otdelen_vw.acting_post
      end rt_post
      
    , case when p_force_self = 0 
      then nvl2(nasel_ccb_sa_rel.spr_cd, nasel_ccb_spr_l.address, nvl2(nasel_ccb_upr.kod_upr, nasel_ccb_upr.address, nasel_acct_otdelen_vw.mailing_address)) 
      else nasel_acct_otdelen_vw.mailing_address
      end rt_addr
    
    , case when p_force_self = 0 
      then nvl2(nasel_ccb_sa_rel.spr_cd, nasel_ccb_spr_l.official_name, nvl2(nasel_ccb_upr.kod_upr, nasel_ccb_upr.director_name, nasel_acct_otdelen_vw.acting_name))
      else nasel_acct_otdelen_vw.acting_name
      end rt_name
      
    , max(trunc(greatest(nvl(cc.cc_dttm + 30, to_date(1721424,'J')), sysdate + 13), 'dd')) over(partition by nasel_ccb_prem.acct_otdelen, nasel_ccb_sa_rel.spr_cd, nasel_ccb_upr.kod_upr) st_p_dt

    /*, dense_rank() over (order by nasel_ccb_prem.acct_otdelen, nasel_ccb_sa_rel.spr_cd, nasel_ccb_upr.kod_upr) rnk
    , nvl2(nasel_ccb_sa_rel.spr_cd, nasel_ccb_spr_l.spr_cd, nvl2(nasel_ccb_upr.kod_upr, nasel_ccb_upr.kod_upr, nasel_acct_otdelen_vw.acct_otdelen)) rt_spr
    , nvl2(nasel_ccb_sa_rel.spr_cd, nasel_ccb_spr_l.descr, nvl2(nasel_ccb_upr.kod_upr, nasel_ccb_upr.upr_descr, nasel_acct_otdelen_vw.descr_l)) rt_spr_cd
    , nvl2(nasel_ccb_sa_rel.spr_cd, nasel_ccb_spr_l.official_post, nvl2(nasel_ccb_upr.kod_upr, nasel_ccb_upr.director_job_descr, nasel_acct_otdelen_vw.acting_post)) rt_post
    , nvl2(nasel_ccb_sa_rel.spr_cd, nasel_ccb_spr_l.address, nvl2(nasel_ccb_upr.kod_upr, nasel_ccb_upr.address, nasel_acct_otdelen_vw.mailing_address)) rt_addr
    , nvl2(nasel_ccb_sa_rel.spr_cd, nasel_ccb_spr_l.official_name, nvl2(nasel_ccb_upr.kod_upr, nasel_ccb_upr.director_name, nasel_acct_otdelen_vw.acting_name)) rt_name*/
    
    
  from nasel_ccb_prem
  inner join nasel_acct_otdelen_vw on nasel_acct_otdelen_vw.acct_otdelen = nasel_ccb_prem.acct_otdelen
  left join nasel_ccb_sa_rel on nasel_ccb_sa_rel.acct_id = nasel_ccb_prem.acct_id and nasel_ccb_sa_rel.sa_rel_type_cd in ('NEP') and nasel_ccb_prem.prem_type_cd in ('CD','DCA', 'GR', 'HOZP', 'PODS', 'STR')
  left join nasel_ccb_spr_l on nasel_ccb_spr_l.spr_cd = nasel_ccb_sa_rel.spr_cd

  left join nasel_ccb_upr on nasel_ccb_upr.prem_id = nasel_ccb_prem.prnt_prem_id and nasel_ccb_upr.tip_uo in ('UK')

  inner join ( -- ��� �������������� ST-P-DT
    select 
      nasel_cc.acct_id
      , max(nasel_cc.cc_dttm) cc_dttm
    from nasel_cc 
    where approval_dttm is not null
    group by nasel_cc.acct_id
  ) cc on cc.acct_id = nasel_ccb_prem.acct_id
  
  where
    nasel_ccb_prem.acct_id in (select val from nasel_list_tmp) 
    --nasel_ccb_prem.acct_id in ('7933750000', '9857960000', '8957960000' , '1710950000' ,'0853750000','3063750000','7400650000','8352750000','6865750000','8781750000'  )

  order by rnk; 
  
  v_res p_cursor%rowtype;
  v_cur_fa_pack_id nasel_fa_pack.fa_pack_id%type;
  v_cur_fa_id nasel_fa.fa_id%type;
  v_cur_grp number;  -- ������ ������ ��� ���������� �� ������� 
  
  v_st_p_dt date;
  
begin
  -- ��������� ������ ���������� �� ��������� ������� 
  for i in p_acct_id_list.first .. p_acct_id_list.last loop
    insert into nasel_list_tmp (val) 
    values (
      p_acct_id_list(i)
    );
  end loop;


  v_cur_grp := 0;
  for v_res in p_cursor
  loop
    if v_cur_grp <> v_res.rnk then  
      if v_cur_fa_pack_id is not null then
        -- ��������� FaPack
        PK_NASEL_SWEETY.SET_FP_STATUS_FLG_FROZEN(v_cur_fa_pack_id);
      end if;
             
      -- ������� FaPack
      v_cur_fa_pack_id := PK_NASEL_SWEETY.CREATE_FP('40', null, v_res.acct_otdelen);
      v_st_p_dt := pk_nasel_otdel.get_workday(v_res.st_p_dt); -- ��������� ��������� ������� ����
      
      -- ��������� �������������� FaPack
      pk_nasel_sweety.add_fp_char_str(v_cur_fa_pack_id, 'RT-TYPE', v_res.rt_type);
      pk_nasel_sweety.add_fp_char_str(v_cur_fa_pack_id, 'RT-SPRCD', v_res.rt_spr_cd);
      pk_nasel_sweety.add_fp_char_str(v_cur_fa_pack_id, 'RT-SPR', v_res.rt_spr);
      pk_nasel_sweety.add_fp_char_str(v_cur_fa_pack_id, 'RT-ADDR', v_res.rt_addr);
      pk_nasel_sweety.add_fp_char_str(v_cur_fa_pack_id, 'RT-NAME', padeg_pack.Padeg_FIO(v_res.rt_name, '�'));
      pk_nasel_sweety.add_fp_char_str(v_cur_fa_pack_id, 'RT-POST', padeg_pack.Padeg_FIO(v_res.rt_post, '�'));
                               
      
      v_cur_grp := v_res.rnk;
    end if;
     
    -- ������� Fa
    v_cur_fa_id := PK_NASEL_SWEETY.CREATE_FA_STOP(v_res.acct_id, v_cur_fa_pack_id);
    -- ��������� �������������� Fa  
    add_fa_char_dttm(v_cur_fa_id, 'ST-P-DT', v_st_p_dt);
   
  end loop;
    
  if v_cur_fa_pack_id is not null then 
    -- ��������� ��������� FaPack - ��������� � ������ ���������
    PK_NASEL_SWEETY.SET_FP_STATUS_FLG_FROZEN(v_cur_fa_pack_id);
    --DBMS_OUTPUT.put_line('FA_PACK frozen: ' || to_char(v_fa_pack_id));
  end if; 

  commit;
end;



-- ��������� ��� ��������������� ����������
-- ������� ������ �� ������ ������ �� ����������� � ������ ��������� ������
-- ��� ���������� ������ � ������, ������������� �������� ����������� ������ � 'PRNT-FA' = �������� ���������� ������
procedure create_fp_cancel_stop
  is
  cursor p_cursor is 
    -- �������� ������ ����������� �� ���������, ���������� ������
  select
    nasel_ccb_prem.acct_id
    , nasel_ccb_prem.acct_otdelen
    , nasel_fa.fa_id
    , nasel_fa_pack.fa_pack_id
    , dense_rank() over (order by  nasel_ccb_prem.acct_otdelen, nasel_ccb_spr.spr_cd, nvl2(fa_char.sa_e_dt,'50','45')) fa_pack_index -- ����� �������� �������� - ��������  nasel_ccb_spr.rt_type 
    , nvl2(fa_char.sa_e_dt,'50','45') fa_pack_type_required -- ����������� ��� �������
  
  from nasel_fa

  inner join nasel_fa_pack on nasel_fa_pack.fa_pack_id = nasel_fa.fa_pack_id and nasel_fa_pack.fa_pack_type_cd = '40' and nasel_fa_pack.fa_pack_status_flg = '50'
 
  -- �������������� ������� - ������ �� ���������
  left join (
    select nasel_fa_pack_char.fa_pack_id 
      , nasel_fa_pack_char.char_val_str rt_spr_cd
    from nasel_fa_pack_char
    where nasel_fa_pack_char.char_type_cd = 'RT-SPRCD'
  ) fa_pack_char on fa_pack_char.fa_pack_id = nasel_fa.fa_pack_id
  
  -- �������������� ���������� �� ���������
  left join (
    select
      nasel_fa_char.fa_id
      , max(case when nasel_fa_char.char_type_cd = 'SA-E-DT' then nasel_fa_char.char_val_dttm end) sa_e_dt
    from nasel_fa_char
    group by nasel_fa_char.fa_id
  ) fa_char on fa_char.fa_id = nasel_fa.fa_id
  
  left join nasel_ccb_spr on nasel_ccb_spr.spr_cd = fa_pack_char.rt_spr_cd
  
  -- ��
  inner join nasel_ccb_prem on nasel_ccb_prem.acct_id = nasel_fa.acct_id

  -- ���������� ����������
  left join (  -- �������� �� left 2017-06-27
    select 
      nasel_ccb_ft.acct_id
      , nasel_ccb_ft.uch_begin_dt
      , nvl(nasel_ccb_ft.bill_bt_otch, 0) + nvl(nasel_ccb_ft.bill_odn_otch, 0) 
        + nvl(nasel_ccb_ft.bill_act_otch, 0) + nvl(nasel_ccb_ft.bill_pen_otch, 0) nach_otch
      , nvl(nasel_ccb_ft.saldo_bt_uch,0) + nvl(nasel_ccb_ft.saldo_odn_uch,0) 
        + nvl(nasel_ccb_ft.saldo_act_uch,0) + nvl(nasel_ccb_ft.saldo_pen_uch,0) saldo_uch
    from nasel_ccb_ft
  ) ccb_ft on ccb_ft.acct_id = nasel_ccb_prem.acct_id
     
  -- ���������������� ��
  left join nasel_ccb_pp on nasel_ccb_pp.acct_id = nasel_ccb_prem.acct_id and nasel_ccb_pp.pp_stat_flg = '20'

  where 
    nasel_fa.fa_id not in ( -- ��������� ��� �������������� ������� �� ������ ��������� � �� �������������
      select nasel_fa.prnt_fa_id --nasel_fa_char.char_val_str 
      from nasel_fa
      inner join nasel_fa_pack on nasel_fa_pack.fa_pack_id = nasel_fa.fa_pack_id and nasel_fa_pack.fa_pack_type_cd in ('45','50') and nasel_fa_pack.fa_pack_status_flg in ('50','60','71')
      where nasel_fa.prnt_fa_id is not null -- 
      --inner join nasel_fa_char on nasel_fa_char.fa_id = nasel_fa.fa_id and nasel_fa_char.char_type_cd = 'PRNT-FA'
    ) 

    --and nasel_fa.fa_id not in (select nasel_fa_char.fa_id from nasel_fa_char where nasel_fa_char.char_type_cd = 'SA-E-DT') -- ��������� ������� ��� ���������

    and ccb_ft.saldo_uch - ccb_ft.nach_otch - nvl(nasel_ccb_pp.pp_tot_sched_amt,0) <= 0 -- ���� ���� ������
    order by nasel_ccb_prem.acct_otdelen, nasel_ccb_spr.spr_cd, nvl2(fa_char.sa_e_dt,'50','45');

  
  v_res p_cursor%rowtype;
  
  v_fa_pack_id nasel_fa_pack.fa_pack_id%type;
  v_fa_pack_index number;  -- ������ ������ ��� ���������� �� �������
  v_fa_id nasel_fa.fa_id%type;
  
  v_fa_pack_char_list t_char_list;
  v_new_fa_pack_char_list t_char_list;
begin
  DBMS_OUTPUT.enable;
  
  v_fa_pack_id := null;
  v_fa_pack_index := 0;
  for v_res in p_cursor
  loop
    if v_fa_pack_index <> v_res.fa_pack_index then -- ������� ������
      if (v_fa_pack_id is not null) then
        set_fp_status_flg_frozen(v_fa_pack_id);  -- ��������� ����������� ������
        DBMS_OUTPUT.put_line('FA_PACK frozen: ' || to_char(v_fa_pack_id));
      end if;
      v_fa_pack_id := create_fp(v_res.fa_pack_type_required, null, v_res.acct_otdelen); -- ������� ������ ���������� ���� (������ ������ ��� �������������)
      v_fa_pack_index := v_res.fa_pack_index;
      
      
      -- ����� ��������� ��������������
      v_fa_pack_char_list := get_fp_char_list(v_res.fa_pack_id);          -- �������� �������������� �� ����������� �������
      
      begin
        if v_fa_pack_char_list.exists('RT-TYPE') then
          add_fp_char_str(v_fa_pack_id, 'RT-TYPE', v_fa_pack_char_list('RT-TYPE'));
          if v_fa_pack_char_list('RT-TYPE') = 'OTDELEN' then -- ���� ���������
            v_new_fa_pack_char_list := get_rt_otdelen(v_res.acct_otdelen);         -- �������� �������������� �������� �� ��������������� �������
              add_fp_char_str(v_fa_pack_id, 'RT-SPR', v_new_fa_pack_char_list('RT-SPR'));
              add_fp_char_str(v_fa_pack_id, 'RT-ADDR', v_new_fa_pack_char_list('RT-ADDR'));
              add_fp_char_str(v_fa_pack_id, 'RT-NAME', v_new_fa_pack_char_list('RT-NAME'));
              add_fp_char_str(v_fa_pack_id, 'RT-POST', v_new_fa_pack_char_list('RT-POST'));      

          elsif v_fa_pack_char_list('RT-TYPE') = 'SPR' then -- ���� ��������� �����������
            DBMS_OUTPUT.put_line(v_res.fa_pack_id);
            
            if v_fa_pack_char_list.exists('RT-SPRCD') then
              v_new_fa_pack_char_list := get_rt_spr(v_fa_pack_char_list('RT-SPRCD'));         -- �������� �������������� �������� �� �������������� �����������
            
                add_fp_char_str(v_fa_pack_id, 'RT-SPRCD', v_fa_pack_char_list('RT-SPRCD'));
                add_fp_char_str(v_fa_pack_id, 'RT-SPR', v_new_fa_pack_char_list('RT-SPR'));
                add_fp_char_str(v_fa_pack_id, 'RT-ADDR', v_new_fa_pack_char_list('RT-ADDR'));
                add_fp_char_str(v_fa_pack_id, 'RT-NAME', v_new_fa_pack_char_list('RT-NAME'));
                add_fp_char_str(v_fa_pack_id, 'RT-POST', v_new_fa_pack_char_list('RT-POST'));
    
            end if;
          end if;
        end if;
      exception 
        when NO_DATA_FOUND then
          null;
      end;      
        
      
      
      DBMS_OUTPUT.put_line('FA_PACK created: ' || to_char(v_fa_pack_id));
    end if;
    
    -- ��������� � ������ ���������
    v_fa_id := create_fa(v_res.acct_id, v_fa_pack_id, v_res.fa_id);
    --add_fa_char_str(v_fa_id, 'PRNT-FA',v_res.fa_id);
    --set_fa_status_flg(v_fa_id, '20');
    --set_fa_status_flg(v_res.fa_id, '60');
    
    DBMS_OUTPUT.put_line('FA ' || to_char(v_fa_id) || ' (acct_id: ' || to_char(v_res.acct_id) || 
                             ') added to PACK: ' || to_char(v_fa_pack_id) 
                        );
  end loop;
  
  if (v_fa_pack_id is not null) then
    set_fp_status_flg_frozen(v_fa_pack_id);  -- ��������� ��������� ��������� ������
       DBMS_OUTPUT.put_line('FA_PACK frozen: ' || to_char(v_fa_pack_id));
  end if;
end;



/* ������������� ������ ������ �� ����������
   ������������, ��������, � ������ ���������� ������ � �������, �� ������� ����������� ��������� �� ������
*/
procedure create_fp_cancel_stop_force(p_fa_id_list in t_fa_id_list)
  is
  cursor p_cursor is 
    -- �������� ������ ����������� �� ���������, ���������� ������
  select
    nasel_ccb_prem.acct_id
    , nasel_ccb_prem.acct_otdelen
    , nasel_fa.fa_id
    , nasel_fa_pack.fa_pack_id
    , dense_rank() over (order by  nasel_ccb_prem.acct_otdelen, nasel_ccb_spr.spr_cd, nvl2(fa_char.sa_e_dt,'50','45')) fa_pack_index -- ����� �������� �������� - ��������  nasel_ccb_spr.rt_type 
    , nvl2(fa_char.sa_e_dt,'50','45') fa_pack_type_required -- ����������� ��� �������
  
  from nasel_fa

  inner join nasel_fa_pack on nasel_fa_pack.fa_pack_id = nasel_fa.fa_pack_id and nasel_fa_pack.fa_pack_type_cd = '40' and nasel_fa_pack.fa_pack_status_flg = '50'
 
  -- �������������� ������� - ������ �� ���������
  left join (
    select nasel_fa_pack_char.fa_pack_id 
      , nasel_fa_pack_char.char_val_str rt_spr_cd
    from nasel_fa_pack_char
    where nasel_fa_pack_char.char_type_cd = 'RT-SPRCD'
  ) fa_pack_char on fa_pack_char.fa_pack_id = nasel_fa.fa_pack_id
  
  -- �������������� ���������� �� ���������
  left join (
    select
      nasel_fa_char.fa_id
      , max(case when nasel_fa_char.char_type_cd = 'SA-E-DT' then nasel_fa_char.char_val_dttm end) sa_e_dt
    from nasel_fa_char
    group by nasel_fa_char.fa_id
  ) fa_char on fa_char.fa_id = nasel_fa.fa_id
  
  left join nasel_ccb_spr on nasel_ccb_spr.spr_cd = fa_pack_char.rt_spr_cd
  
  -- ��
  inner join nasel_ccb_prem on nasel_ccb_prem.acct_id = nasel_fa.acct_id

  -- ���������� ����������
  /*inner join (
    select 
      nasel_ccb_ft.acct_id
      , nasel_ccb_ft.uch_begin_dt
      , nvl(nasel_ccb_ft.bill_bt_otch, 0) + nvl(nasel_ccb_ft.bill_odn_otch, 0) 
        + nvl(nasel_ccb_ft.bill_act_otch, 0) + nvl(nasel_ccb_ft.bill_pen_otch, 0) nach_otch
      , nvl(nasel_ccb_ft.saldo_bt_uch,0) + nvl(nasel_ccb_ft.saldo_odn_uch,0) 
        + nvl(nasel_ccb_ft.saldo_act_uch,0) + nvl(nasel_ccb_ft.saldo_pen_uch,0) saldo_uch
    from nasel_ccb_ft
  ) ccb_ft on ccb_ft.acct_id = nasel_ccb_prem.acct_id*/
     
  -- ���������������� ��
  left join nasel_ccb_pp on nasel_ccb_pp.acct_id = nasel_ccb_prem.acct_id and nasel_ccb_pp.pp_stat_flg = '20'

  where 
    nasel_fa.fa_id not in ( -- ��������� ��� �������������� ������� �� ������ ��������� � �� �������������
      select nasel_fa.prnt_fa_id --nasel_fa_char.char_val_str 
      from nasel_fa
      inner join nasel_fa_pack on nasel_fa_pack.fa_pack_id = nasel_fa.fa_pack_id and nasel_fa_pack.fa_pack_type_cd in ('45','50') and nasel_fa_pack.fa_pack_status_flg in ('50','60','71')
      where nasel_fa.prnt_fa_id is not null -- 
      --inner join nasel_fa_char on nasel_fa_char.fa_id = nasel_fa.fa_id and nasel_fa_char.char_type_cd = 'PRNT-FA'
    ) 

    --and ccb_ft.saldo_uch - ccb_ft.nach_otch - nvl(nasel_ccb_pp.pp_tot_sched_amt,0) <= 0 -- ���� ���� ������
    and nasel_fa.fa_id in (select val from nasel_list_tmp)
   
    order by nasel_ccb_prem.acct_otdelen, nasel_ccb_spr.spr_cd, nvl2(fa_char.sa_e_dt,'50','45');

  
  v_res p_cursor%rowtype;
  
  v_fa_pack_id nasel_fa_pack.fa_pack_id%type;
  v_fa_pack_index number;  -- ������ ������ ��� ���������� �� �������
  v_fa_id nasel_fa.fa_id%type;
  
  v_fa_pack_char_list t_char_list;
  v_new_fa_pack_char_list t_char_list;
  
  
begin
  
  --
  /*for v_res in p_cursor
  loop
    insert into kavabunga(name) 
    values (
    v_res.acct_id);
  end loop;

  commit; */ 
  
  
  /*FOR r_emp IN c_emp LOOP
    v_dept_aa(r_emp.deptNo).extra_tx:=
    v_dept_aa(r_emp.deptNo).extra_tx||' '||r_emp.eName;
  END LOOP;

  for i in 1..dbfields.COUNT loop
  end loop;*/
  
 
  DBMS_OUTPUT.enable;
  
  -- ��������� ������ ���������� �� ��������� ������� 
  for i in p_fa_id_list.first .. p_fa_id_list.last loop
    insert into nasel_list_tmp(val) 
    values (
    p_fa_id_list(i));
  end loop;

  --
  v_fa_pack_id := null;
  v_fa_pack_index := 0;
  for v_res in p_cursor
  loop
    if v_fa_pack_index <> v_res.fa_pack_index then -- ������� ������
      if (v_fa_pack_id is not null) then
        pk_nasel_sweety.set_fp_status_flg_frozen(v_fa_pack_id);  -- ��������� ����������� ������
        DBMS_OUTPUT.put_line('FA_PACK frozen: ' || to_char(v_fa_pack_id));
      end if;
      v_fa_pack_id := pk_nasel_sweety.create_fp(v_res.fa_pack_type_required, null, v_res.acct_otdelen); -- ������� ������ ���������� ���� (������ ������ ��� �������������)
      v_fa_pack_index := v_res.fa_pack_index;
      
      
      -- ����� ��������� ��������������
      v_fa_pack_char_list := get_fp_char_list(v_res.fa_pack_id);          -- �������� �������������� �� ����������� �������
      
      begin
        if v_fa_pack_char_list.exists('RT-TYPE') then
          add_fp_char_str(v_fa_pack_id, 'RT-TYPE', v_fa_pack_char_list('RT-TYPE'));
          if v_fa_pack_char_list('RT-TYPE') = 'OTDELEN' then -- ���� ���������
            v_new_fa_pack_char_list := get_rt_otdelen(v_res.acct_otdelen);         -- �������� �������������� �������� �� ��������������� �������
              add_fp_char_str(v_fa_pack_id, 'RT-SPR', v_new_fa_pack_char_list('RT-SPR'));
              add_fp_char_str(v_fa_pack_id, 'RT-ADDR', v_new_fa_pack_char_list('RT-ADDR'));
              add_fp_char_str(v_fa_pack_id, 'RT-NAME', v_new_fa_pack_char_list('RT-NAME'));
              add_fp_char_str(v_fa_pack_id, 'RT-POST', v_new_fa_pack_char_list('RT-POST'));      

          elsif v_fa_pack_char_list('RT-TYPE') = 'SPR' then -- ���� ��������� �����������
            DBMS_OUTPUT.put_line(v_res.fa_pack_id);
            
            if v_fa_pack_char_list.exists('RT-SPRCD') then
              v_new_fa_pack_char_list := get_rt_spr(v_fa_pack_char_list('RT-SPRCD'));         -- �������� �������������� �������� �� �������������� �����������
            
                add_fp_char_str(v_fa_pack_id, 'RT-SPRCD', v_fa_pack_char_list('RT-SPRCD'));
                add_fp_char_str(v_fa_pack_id, 'RT-SPR', v_new_fa_pack_char_list('RT-SPR'));
                add_fp_char_str(v_fa_pack_id, 'RT-ADDR', v_new_fa_pack_char_list('RT-ADDR'));
                add_fp_char_str(v_fa_pack_id, 'RT-NAME', v_new_fa_pack_char_list('RT-NAME'));
                add_fp_char_str(v_fa_pack_id, 'RT-POST', v_new_fa_pack_char_list('RT-POST'));
    
            end if;
          end if;
        end if;
      exception 
        when NO_DATA_FOUND then
          null;
      end;      
        
      
      
      DBMS_OUTPUT.put_line('FA_PACK created: ' || to_char(v_fa_pack_id));
    end if;
    
    -- ��������� � ������ ���������
    v_fa_id := create_fa(v_res.acct_id, v_fa_pack_id, v_res.fa_id);
    --add_fa_char_str(v_fa_id, 'PRNT-FA',v_res.fa_id);
    --set_fa_status_flg(v_fa_id, '20');
    --set_fa_status_flg(v_res.fa_id, '60');
    
    DBMS_OUTPUT.put_line('FA ' || to_char(v_fa_id) || ' (acct_id: ' || to_char(v_res.acct_id) || 
                             ') added to PACK: ' || to_char(v_fa_pack_id) 
                        );
  end loop;
  
  if (v_fa_pack_id is not null) then
    set_fp_status_flg_frozen(v_fa_pack_id);  -- ��������� ��������� ��������� ������
       DBMS_OUTPUT.put_line('FA_PACK frozen: ' || to_char(v_fa_pack_id));
  end if;
end;








 

/*************************************
**************************************/


-- ����� ���������������� ����������
procedure get_app_config(p_app_path in varchar2, p_app_ver in varchar2, rc out sys_refcursor)
is
begin
  open rc for 
  
  select
    t.*
    , case COALESCE(other_stop_reason, update_failure, needs_app_update) 
      when '20' then '������ ����� ��������� ��������.'
      when '30' then '�� ����� ������������ ���� ������ ��������� ����.'
      when '40' then t.service_stop_allert
      end stop_allert
  from
  (
    select
      t.*
      
      -- ���������� �������� 
      , case when to_date(p_app_ver, 'yyyymmddhh24miss') < to_date(min_app_ver, 'yyyymmddhh24miss') 
          or service_stop_allert is not null
        then 0 else 1 end allowed
      
      -- �������� ������� ���������� ������ ���������
      , case when to_date(p_app_ver, 'yyyymmddhh24miss') < to_date(min_app_ver, 'yyyymmddhh24miss') 
        then '20' end needs_app_update  -- ������ ��������
          
      , ( -- �������� ������� ���������� ����������
        select
          case when max(case when cfg_task.char_type_cd = 'FAILURE' then cfg_task.char_val_dttm end) >
                max(case when cfg_task.char_type_cd = 'LAST_UPD' then cfg_task.char_val_dttm end)
          then '30' end
        from cfg_task
        where cfg_task.task_name = 'P_UPDATE_NASEL_JOB_DAY' 
        group by null) update_failure -- ��������� ������ ��� ���������� ������
      
      -- ������ ������� ���������  
      , case when service_stop_allert is not null then '40' end other_stop_reason
          
    from(
      select sysdate
        , p_app_path app_path
        , p_app_path || 'report\' report_path
        , p_app_path || 'report\visa\' visa_path
        --, :app_path || 'report\template_document_notice.dotx'
        , sys_context('USERENV','SESSION_USER') username
        , 201705231500 min_app_ver                  -- ���������� ���������� ������
        , 20991231 max_app_ver                  -- ����������� ���������� ������
        , sysdate today -- ������� �����
        ,  /*'������������ ���������� ���������.'*/'' service_stop_allert -- ������� ���� ���� ������� ��������� ���������
      from dual
    ) t
  ) t;      
  
  v_user_app_ver_dt := to_date(p_app_ver, 'yyyymmddhh24miss'); 
  
  /*update rc 
  set rc.allowed = 2;*/
  --where nasel_fa_pack.fa_pack_id = p_fa_pack_id;
end;

/* ������ ��������� */
procedure get_acct_otdelen_list(rc out sys_refcursor)
is
begin
  open rc for 
  -- ����� ���������� �� ��������
  select nasel_acct_otdelen_vw.acct_otdelen
    , nasel_acct_otdelen_vw.descr
    , nasel_acct_otdelen_vw.mailing_address 
    , nasel_acct_otdelen_vw.acting_name
    , nasel_acct_otdelen_vw.acting_post
    , nasel_acct_otdelen_vw.descr_l_genitive
    , nasel_acct_otdelen_vw.phone
    , nasel_acct_otdelen_vw.descr_l
    
    -- ���� ���� �������
    , nasel_acct_otdelen_vw.descr otdelen_descr
    , nasel_acct_otdelen_vw.mailing_address address
    , nasel_acct_otdelen_vw.acting_name nachalnik
    , nasel_acct_otdelen_vw.acting_post post
    , null ccb_acct_char_val
    
  from nasel_acct_otdelen_vw
  inner join acc_grp_dar on acc_grp_dar.dar_cd = nasel_acct_otdelen_vw.dar_cd
  inner join spr_users on spr_users.access_grp_cd = acc_grp_dar.access_grp_cd
  where spr_users.name_u = sys_context('USERENV','SESSION_USER')
  order by nasel_acct_otdelen_vw.acct_otdelen;
  
  /*select nasel_uch.descr otdelen_descr 
    , nasel_uch.acct_otdelen
    , raion.address
    , raion.nachalnik
    , raion.phone
    , raion.ccb_acct_char_val
    --, :app_path || 'report\visa\'|| raion.nachalnik || '.png' visa
    , substr(trim(raion.FNAME),1,length(trim(raion.FNAME))-2) || '��� �������'  post
    , nasel_uch.descr_l 
   
  from acc_grp_dar
  inner join spr_users on spr_users.access_grp_cd = acc_grp_dar.access_grp_cd
  inner join nasel_uch on nasel_uch.dar_cd = acc_grp_dar.dar_cd

  inner join raion on raion.id = nasel_uch.id_rn
  --where name_u = 'U062000'
  where name_u = sys_context('USERENV','SESSION_USER');*/
end;



/* ���������� ������ ����������� */
procedure get_op_area_cd_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor)
is
begin
  open rc for 
    select nasel_ccb_sp.op_area_cd
      , initcap(nasel_ccb_sp.op_area_descr) op_area_descr
      , nasel_ccb_prem.acct_otdelen  
    from nasel_ccb_prem
    inner join nasel_ccb_sp on nasel_ccb_sp.acct_id = nasel_ccb_prem.acct_id
    where nasel_ccb_prem.acct_otdelen = p_acct_otdelen
    group by op_area_cd, nasel_ccb_sp.op_area_descr, nasel_ccb_prem.acct_otdelen
    order by op_area_descr;
end;

/* ���������� ������ ����� �������� */
procedure get_fp_type_list(rc out sys_refcursor)
is
begin
  open rc for 
  select 
    nasel_lookup_val.field_value fa_pack_type_cd
    , nasel_lookup_val.descr
  from nasel_lookup_val
  where field_name = 'FA_PACK_TYPE_CD'
    and eff_status = 'A';
end;

/* �������� ���������� �� ��������*/
procedure get_fp_stats(p_acct_otdelen nasel_ccb_prem.acct_otdelen%type, p_start_dt date, p_end_dt date, rc out sys_refcursor)
is
begin
  open rc for

    select 
      decode(grouping(nasel_uch.acct_otdelen),0, nasel_uch.acct_otdelen, '') acct_otdelen
      , decode(grouping(nasel_uch.acct_otdelen),0, max(nasel_uch.descr), '�����') acct_otdelen_descr
      , count(case when nasel_fa_pack.fa_pack_type_cd in ('10') then nasel_fa.fa_id end) fa_notices_self 
      , count(case when nasel_fa_pack.fa_pack_type_cd in ('20') then nasel_fa.fa_id end) fa_notices_post
      , count(case when nasel_fa_pack.fa_pack_type_cd in ('40') then nasel_fa.fa_id end) fa_pack_stop
      , count(case when nasel_fa_pack.fa_pack_type_cd in ('45') then nasel_fa.fa_id end) fa_pack_cancel
      , count(case when nasel_fa_pack.fa_pack_type_cd in ('50') then nasel_fa.fa_id end) fa_pack_reconnect

    from nasel_uch

    left join nasel_fa_pack on nasel_fa_pack.acct_otdelen = nasel_uch.acct_otdelen 
      and nasel_fa_pack.fa_pack_status_flg not in ('10','20')
      and trunc(nasel_fa_pack.cre_dttm,'dd') between trunc(p_start_dt,'dd') and trunc(nvl(p_end_dt, sysdate),'dd') --trunc(sysdate,'mm')-1 

    left join nasel_fa on nasel_fa.fa_pack_id = nasel_fa_pack.fa_pack_id
     
    where nasel_uch.acct_otdelen = p_acct_otdelen or p_acct_otdelen is null
    group by rollup(nasel_uch.acct_otdelen);

end;



/* ���������� ���������� �� ������� */
procedure get_fp_inf(p_fa_pack_id in nasel_fa_pack.fa_pack_id%type, rc out sys_refcursor)
is
begin
  open rc for
  select
    /*rownum
    , sysdate*/
    t.cre_dttm
    , t.acct_otdelen
    , t.fa_pack_id
    , t.fa_count
    
    --, t.acct_otdelen_descr
    --, t.address
    --, t.post
    --, t.phone
    --, t.nachalnik  
    --, t.visa

  from (
    select
      nasel_fa_pack.* 
      , nasel_uch.fname
      , raion.fname acct_otdelen_descr
      , raion.address
      , raion.nachalnik
      , raion.phone
      , raion.ccb_acct_char_val       
      --, :app_path || 'report\visa\'|| raion.nachalnik || '.png' visa
      --, substr(trim(raion.FNAME),1,length(trim(raion.FNAME))-2) || '��� �������'  post
      , fa.fa_count
    from nasel_fa_pack
    left join nasel_uch on nasel_uch.acct_otdelen = nasel_fa_pack.acct_otdelen
    left join raion on raion.id = nasel_uch.id_rn

    left join (    
      select
        nasel_fa.fa_pack_id
        , count(*) fa_count 
      from nasel_fa
      group by nasel_fa.fa_pack_id
    ) fa on fa.fa_pack_id = nasel_fa_pack.fa_pack_id
   
    where nasel_fa_pack.fa_pack_id = p_fa_pack_id --'0000000416'
    order by nasel_fa_pack.cre_dttm desc 
  ) t;
end;

/* ���������� ���������� �� �������� */
procedure get_fa_cc_inf(p_cc_id in nasel_cc.cc_id%type, rc out sys_refcursor)
is
begin
  open rc for
  select
    *
  from nasel_cc
  where nasel_cc.cc_id = p_cc_id;
end;


/* ������ ���� ��������� */
procedure get_acct_full_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor)
is
begin
  open rc for 
  -- ��� ��������
  -- 2017-02-16
  -- �����: ������������
  --
  -- �������
  -- 1. ����� ����� � ������� ����������� (20)
  -- 2. saldo_uch > normativ * 2 and saldo_m3 > normativ * 2
  -- 3. ���� ���������� ������������� �������� ����� 6 ������� ��� ������
  -- 4. ���� ���������� ����������� ����� 2 ������� ��� ������

  select
    rownum 
    , 0 CHECK_DATA 
    , acct_id  
    , initcap(fio) fio   
    , city
    , address 
    , prem_type_descr

    , saldo_uch 
    , saldo_m3 
    , cc_dttm  
    , fa_id
    , fa_pack_id
    , cre_dttm 
    , acct_otdelen
    , service_org

    , op_area_descr 

    , case when saldo_uch - bill_otch > norm_amt * 2 * fl_tar11 and saldo_m3 > norm_amt * 2 * fl_tar11 and saldo_uch > norm_amt * 2 * fl_tar11 
          and (cc_dttm is null or months_between(cc_dttm, sysdate) > 6) 
          and (cre_dttm is null or months_between(cre_dttm, sysdate) > 2)
      then 1 else 0 end debtor_flg

     
  from (
  select
    *
  from (
    select
      nasel_ccb_prem.acct_id  
      , nasel_ccb_prem.acct_otdelen
      , nasel_ccb_prem.fio   
      , nasel_ccb_prem.city
      , nasel_ccb_prem.address
      , (select descr from nasel_lookup_val where field_name = 'PREM_TYPE_CD' and field_value = nasel_ccb_prem.prem_type_cd) prem_type_descr
      
      , nvl(nasel_ccb_ft.bill_bt_otch,0) + nvl(nasel_ccb_ft.bill_odn_otch,0) bill_otch
      , nvl(nasel_ccb_ft.saldo_bt_uch, 0) + nvl(nasel_ccb_ft.saldo_odn_uch, 0) + nvl(nasel_ccb_ft.saldo_act_uch, 0) saldo_uch
      , nvl(nasel_ccb_ft_hist.saldo_bt_uch, 0) + nvl(nasel_ccb_ft_hist.saldo_odn_uch, 0) + nvl(nasel_ccb_ft_hist.saldo_act_uch, 0) saldo_m3
      --, nasel_ccb_ft_hist.saldo_odn_uch
        
      , last_cc.cc_dttm
      , last_cc.cc_id
      , last_fa.fa_id
      , last_fa.fa_pack_id
      , last_fa.cre_dttm 
      , nasel_ccb_spr.descr service_org
      , nasel_ccb_sp.op_area_descr
      , nasel_ccb_sp.fl_tar11
      , nasel_calc.norm_amt
    

    from nasel_ccb_prem

    inner join nasel_ccb_sp on nasel_ccb_sp.acct_id = nasel_ccb_prem.acct_id and nasel_ccb_sp.sa_status_flg in ('20')
    left join nasel_ccb_ft on nasel_ccb_ft.acct_id = nasel_ccb_prem.acct_id --and nasel_ccb_ft_hist.calc_dt = trunc(add_months(sysdate,-1), 'mm')
    left join nasel_ccb_ft_hist on nasel_ccb_ft_hist.acct_id = nasel_ccb_prem.acct_id 
      and nasel_ccb_ft_hist.uch_begin_dt = add_months((select begin_dt from nasel_ccb_cal),-3)
      
      
    left join (
      select 
        nasel_cc.acct_id  
        , nasel_cc.cc_dttm 
        , nasel_cc.src_id
        , nasel_cc.cc_id
        , row_number() over (partition by nasel_cc.acct_id order by nasel_cc.cc_dttm desc) n
      from nasel_cc        
      where nasel_cc.src_type_cd = '10' and nasel_cc.approval_dttm is not null  
    ) last_cc on last_cc.acct_id = nasel_ccb_prem.acct_id and last_cc.n = 1    
    
    left join (
      select
        nasel_fa.acct_id  
        , nasel_fa.fa_id
        , nasel_fa.fa_pack_id     
        , nasel_fa_pack.cre_dttm
        , row_number() over (partition by nasel_fa.acct_id order by nasel_fa_pack.cre_dttm desc nulls last) n
      from nasel_fa
      inner join nasel_fa_pack on nasel_fa_pack.fa_pack_id = nasel_fa.fa_pack_id
    ) last_fa on last_fa.acct_id = nasel_ccb_prem.acct_id and last_fa.n = 1    
       
    left join (
      select nasel_ccb_sa_rel.acct_id
        , nasel_ccb_sa_rel.spr_cd
        , row_number() over (partition by  nasel_ccb_sa_rel.acct_id order by nasel_ccb_sa_rel.sa_rel_type_cd desc) n
      from nasel_ccb_sa_rel 
    ) sa_rel on sa_rel.acct_id = nasel_ccb_prem.acct_id and sa_rel.n = 1
    left join nasel_ccb_spr on nasel_ccb_spr.spr_cd = sa_rel.spr_cd
    
    left join nasel_calc on nasel_calc.acct_id = nasel_ccb_prem.acct_id

    where acct_otdelen = p_acct_otdelen
      /*and acct_id in ('4015250000', '4046250000', '6046250000','1441170000','0000003304', '7474750000','7204750000','4006750000','6333850000','1390950000', '6326650000', '3043850000', '9226650000', 
        '0408650000', '1608650000', '1783850000' , '4595850000') /**/
    
  )     
  order by city, address
  );
end;



/*  ��������������� ������ ����������� - ������ ��������� */
procedure get_pre_notices_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor)
is
begin
  open rc for 
-- ��� ��������
-- 2017-02-16
-- �����: ������������
-- �.�.�������
--
-- �������
-- 1. ����� ����� � ������� ����������� (20)
-- 2. saldo_uch > normativ * 2 and saldo_m3 > normativ * 2
-- 3. ���� ���������� ������������� �������� ����� 6 ������� ��� ������
-- 4. ���� ���������� ����������� ����� 2 ������� ��� ������

select
  rownum 
  , 0 CHECK_DATA 
  , acct_id  
  , initcap(fio) fio   
  , city
  , address 
  , prem_type_descr

  , saldo_uch 
  , saldo_m3 
  , cc_dttm  
  , fa_id
  , fa_pack_id
  , cre_dttm 
  , acct_otdelen
  , service_org

  , op_area_descr
  , mr_rte_cd
   
from (
  select
    nasel_ccb_prem.acct_id  
    , nasel_ccb_prem.acct_otdelen
    , nasel_ccb_prem.fio   
    , nasel_ccb_prem.city
    , nasel_ccb_prem.ulitsa || nvl2(nasel_ccb_prem.dom, ', �. '  || nasel_ccb_prem.dom,'') || nvl2(nasel_ccb_prem.korp, ', ����. '  || nasel_ccb_prem.korp,'') || nvl2(nasel_ccb_prem.kvartira, ', ��. '  || nasel_ccb_prem.kvartira,'') address
    , (select descr from nasel_lookup_val where field_name = 'PREM_TYPE_CD' and field_value = nasel_ccb_prem.prem_type_cd) prem_type_descr
    , nvl(ccb_ft.bill_otch, 0) bill_otch
    --, nvl(nasel_ccb_ft.saldo_bt_uch, 0) + nvl(nasel_ccb_ft.saldo_odn_uch, 0) + nvl(nasel_ccb_ft.saldo_act_uch, 0) saldo_uch
    , ccb_ft.saldo_uch
    , ccb_ft_hist.saldo_m3
    --, nvl(nasel_ccb_ft_hist.saldo_bt_uch, 0) + nvl(nasel_ccb_ft_hist.saldo_odn_uch, 0) + nvl(nasel_ccb_ft_hist.saldo_act_uch, 0) saldo_m3
    --, nasel_ccb_ft_hist.saldo_odn_uch
      
    , last_cc.cc_dttm
    , last_cc.cc_id
    , last_fa.fa_id
    , last_fa.fa_pack_id
    , last_fa.cre_dttm 
    , nasel_ccb_spr.descr service_org
    , nasel_ccb_sp.op_area_descr
    , nasel_ccb_sp.fl_tar11
    , nasel_ccb_sp.mr_rte_cd
    , nasel_calc.norm_amt

  from nasel_ccb_prem

  inner join nasel_ccb_sp on nasel_ccb_sp.acct_id = nasel_ccb_prem.acct_id and nasel_ccb_sp.sa_status_flg in ('20')
  
  --left join nasel_ccb_ft on nasel_ccb_ft.acct_id = nasel_ccb_prem.acct_id --and nasel_ccb_ft_hist.calc_dt = trunc(add_months(sysdate,-1), 'mm')
  left join (
    select 
      nasel_ccb_ft.acct_id
      , nasel_ccb_ft.uch_begin_dt
      , nvl(nasel_ccb_ft.bill_bt_otch,0) + nvl(nasel_ccb_ft.bill_odn_otch,0) bill_otch -- ������ ��� �� �������� �.�������� 2017-04-21
      , nvl(nasel_ccb_ft.saldo_bt_uch,0) + nvl(nasel_ccb_ft.saldo_odn_uch,0) + nvl(nasel_ccb_ft.saldo_act_uch,0)  saldo_uch
      , nvl(nasel_ccb_ft.saldo_bt_uch,0) + nvl(nasel_ccb_ft.saldo_act_uch,0) saldo_uch_without_odn
      , nvl(nasel_ccb_ft.sum_pay_uch,0) sum_pay_uch
    from nasel_ccb_ft
  ) ccb_ft on ccb_ft.acct_id = nasel_ccb_prem.acct_id
  
  
  left join (
    select
      nasel_ccb_ft_hist.acct_id
      --, max(case when nasel_ccb_ft_hist.uch_begin_dt = trunc(add_months((select char_val_dttm from cfg_task where char_type_cd = 'REP_DTTM' and task_name = 'P_UPDATE_NASEL_JOB_DAY'),-3), 'mm') then nvl(nasel_ccb_ft_hist.saldo_bt_uch, 0) + nvl(nasel_ccb_ft_hist.saldo_act_uch, 0) end) saldo_m3
      --, nvl(sum(case when nasel_ccb_ft_hist.uch_begin_dt between trunc(add_months((select char_val_dttm from cfg_task where char_type_cd = 'REP_DTTM' and task_name = 'P_UPDATE_NASEL_JOB_DAY'),-2), 'mm') and sysdate
      , max(case when nasel_ccb_ft_hist.uch_begin_dt = add_months((select begin_dt from nasel_ccb_cal),-3) then nvl(nasel_ccb_ft_hist.saldo_bt_uch, 0) + nvl(nasel_ccb_ft_hist.saldo_act_uch, 0) end) saldo_m3
      , nvl(sum(case when nasel_ccb_ft_hist.uch_begin_dt between add_months((select begin_dt from nasel_ccb_cal),-2) and sysdate
      then nasel_ccb_ft_hist.sum_pay_uch end),0) sum_pay_uch
    from nasel_ccb_ft_hist
    group by nasel_ccb_ft_hist.acct_id
  ) ccb_ft_hist on ccb_ft_hist.acct_id = nasel_ccb_prem.acct_id

  
  /*left join nasel_ccb_ft_hist on nasel_ccb_ft_hist.acct_id = nasel_ccb_prem.acct_id and nasel_ccb_ft_hist.calc_dt = trunc(add_months((select char_val_dttm from cfg_task where char_type_cd = 'REP_DTTM' and task_name = 'P_UPDATE_NASEL_JOB_DAY'),-3), 'mm')*/
    
  left join (
    select 
      nasel_cc.acct_id  
      , nasel_cc.cc_dttm 
      , nasel_cc.src_id
      , nasel_cc.cc_id
      , row_number() over (partition by nasel_cc.acct_id order by nasel_cc.cc_dttm desc) n
    from nasel_cc        
    where nasel_cc.src_type_cd = '10' and nasel_cc.approval_dttm is not null  
  ) last_cc on last_cc.acct_id = nasel_ccb_prem.acct_id and last_cc.n = 1    
  
  -- ��������� �����������
  left join (
    select
      nasel_fa.acct_id  
      , nasel_fa.fa_id
      , nasel_fa.fa_pack_id     
      , nasel_fa_pack.cre_dttm
      , row_number() over (partition by nasel_fa.acct_id order by nasel_fa_pack.cre_dttm desc nulls last) n
    from nasel_fa
    inner join nasel_fa_pack on nasel_fa_pack.fa_pack_id = nasel_fa.fa_pack_id
      and nasel_fa_pack.fa_pack_status_flg not in ('10','20')
      and nasel_fa_pack.fa_pack_type_cd in ('10','20') -- �����������
  ) last_fa on last_fa.acct_id = nasel_ccb_prem.acct_id and last_fa.n = 1    
     
  left join (
    select nasel_ccb_sa_rel.acct_id
      , nasel_ccb_sa_rel.spr_cd
      , row_number() over (partition by  nasel_ccb_sa_rel.acct_id order by nasel_ccb_sa_rel.sa_rel_type_cd desc) n
    from nasel_ccb_sa_rel 
  ) sa_rel on sa_rel.acct_id = nasel_ccb_prem.acct_id and sa_rel.n = 1
  left join nasel_ccb_spr on nasel_ccb_spr.spr_cd = sa_rel.spr_cd

  left join nasel_calc on nasel_calc.acct_id = nasel_ccb_prem.acct_id
  
  -- ���������������� ��
  left join nasel_ccb_pp on nasel_ccb_pp.acct_id = nasel_ccb_prem.acct_id and nasel_ccb_pp.pp_stat_flg = '20'

  where nasel_ccb_prem.acct_otdelen = p_acct_otdelen
    /*and nasel_ccb_prem.acct_id in ('4015250000', '4046250000', '6046250000','1441170000','0000003304', '7474750000','7204750000','4006750000','6333850000','1390950000', '6326650000', '3043850000', '9226650000', 
      '0408650000', '1608650000', '1783850000' , '4595850000') /**/
  
    and nvl(ccb_ft.saldo_uch_without_odn,0) - nvl(ccb_ft.bill_otch,0) > nasel_calc.norm_amt * 2 * nasel_ccb_sp.fl_tar11 
    and nvl(ccb_ft_hist.saldo_m3,0) - nvl(ccb_ft_hist.sum_pay_uch,0) - nvl(ccb_ft.sum_pay_uch,0) > nasel_calc.norm_amt * 2 * nasel_ccb_sp.fl_tar11 
    and nvl(ccb_ft.saldo_uch_without_odn,0) > nasel_calc.norm_amt * 2 * nasel_ccb_sp.fl_tar11 
    and (last_cc.cc_dttm is null or months_between(last_cc.cc_dttm, sysdate) > 3) 
    and (last_fa.cre_dttm is null or months_between(last_fa.cre_dttm, sysdate) > 2 
      or (last_cc.cc_dttm is null and trunc(sysdate) - trunc(last_fa.cre_dttm) > 10)
    )
    and (ccb_ft.saldo_uch - nvl(nasel_ccb_pp.pp_tot_sched_amt,0) ) > nasel_calc.norm_amt * 2 * nasel_ccb_sp.fl_tar11
    
  order by city, address
);
end;



/* ���������� ������ ����������� � ������� */
procedure get_fp_notices_content(p_fa_pack_id in nasel_fa_pack.fa_pack_id%type, rc out sys_refcursor)
is
begin
  open rc for 
select
  rownum
  , 0 CHECK_DATA
  , t.*
from (
select 
  nasel_fa.fa_id
  , nasel_fa.acct_id
  , nasel_ccb_prem.ulitsa || nvl2(nasel_ccb_prem.dom, ', �. '  || nasel_ccb_prem.dom,'') || nvl2(nasel_ccb_prem.korp, ', ����. '  || nasel_ccb_prem.korp,'') || nvl2(nasel_ccb_prem.kvartira, ', ��. '  || nasel_ccb_prem.kvartira,'') address
  --, nasel_ccb_prem.address

  , nasel_ccb_prem.city
  , initcap(nasel_ccb_prem.fio) fio 
  , nasel_ccb_prem.postal

  , cc.cc_dttm   
  , cc.src_type_cd
  , nasel_ccb_spr.descr service_org 
  , nasel_ccb_prem.phones
  , trim(substr(nasel_ccb_prem.phones, 1, instr(nasel_ccb_prem.phones,',')-1)) phone
  
  
  , case when nasel_ccb_sp.cont_qty_sz = 1 then nvl(nasel_fa.end_reg_reading1, 0) + nvl(nasel_fa.end_reg_reading2, 0) end end_reg_reading_sz1
  , nvl(nasel_fa.end_reg_reading1, 0) end_reg_reading1
  , nvl(nasel_fa.end_reg_reading2, 0) end_reg_reading2

  --, nvl(nasel_ccb_ft_hist.saldo_bt_uch, 0) + nvl(nasel_ccb_ft_hist.saldo_odn_uch, 0) + nvl(nasel_ccb_ft_hist.saldo_a�t_uch, 0) saldo_m3
  , nvl(nasel_fa.saldo_uch, 0) saldo_uch
  , nasel_fa.mtr_serial_nbr 
  , nasel_fa_pack.fa_pack_id 
  , nasel_fa_pack.acct_otdelen 
  , nasel_fa_pack.cre_dttm
  , nasel_ccb_sp.mr_rte_cd
  , nasel_ccb_sp.op_area_descr
  , nasel_fa_pack.fa_pack_type_cd
  , nvl(nasel_ccb_sp.cont_qty_sz,0) cont_qty_sz
  , cc.cc_id
  , cc.cc_status_flg
  , cc.cc_type_cd
  , trunc(cc.approval_dttm,'dd') approval_dttm
  --, case when cc.approval_dttm is null then '���' else '��' end approval_flg
  --, (select name1 || substr(name2,1,1) || substr(name3,1,1)  from spr_users where name_u = trim(cc.approval_user_id)) approval_user 
  , (select name1 || substr(name2,1,1) || substr(name3,1,1)  from spr_users where name_u = trim(nasel_fa_pack.user_id)) owner 

  --, to_char(nasel_fa_pack.cre_dttm, 'dd.mm.yyyy') cre_dt
  --, nasel_ccb_prem.ulitsa
  --, nasel_ccb_prem.dom
  --, nasel_ccb_prem.korp
  --, nasel_ccb_prem.kvartira
  
from nasel_fa_pack
inner join nasel_fa on nasel_fa.fa_pack_id = nasel_fa_pack.fa_pack_id
inner join nasel_ccb_prem on nasel_ccb_prem.acct_id = nasel_fa.acct_id
inner join nasel_ccb_sp on nasel_ccb_sp.acct_id = nasel_ccb_prem.acct_id

--left join nasel_ccb_bseg_read on nasel_ccb_bseg_read.acct_id = nasel_ccb_prem.acct_id 
--left join nasel_ccb_ft_hist on nasel_ccb_ft_hist.acct_id = nasel_ccb_prem.acct_id and nasel_ccb_ft_hist.calc_dt = trunc(add_months(sysdate,-2), 'mm')
--inner join nasel_ccb_ft on nasel_ccb_ft.acct_id = nasel_fa.acct_id
  
left join (
  select nasel_ccb_sa_rel.acct_id
    , nasel_ccb_sa_rel.spr_cd
    , row_number() over (partition by  nasel_ccb_sa_rel.acct_id order by nasel_ccb_sa_rel.sa_rel_type_cd desc) n
  from nasel_ccb_sa_rel 
  --where acct_id in ('0129250000')
) sa_rel on sa_rel.acct_id = nasel_ccb_prem.acct_id and sa_rel.n = 1
left join nasel_ccb_spr on nasel_ccb_spr.spr_cd = sa_rel.spr_cd

left join (
  select 
     nasel_cc.src_id
    , nasel_cc.cc_dttm
    , nasel_cc.descr
    , nasel_cc.src_type_cd 
    , nasel_cc.cc_id
    , nasel_cc.cc_status_flg
    , nasel_cc.cc_type_cd
    , nasel_cc.approval_dttm
    , nasel_cc.approval_user_id
    , row_number() over (partition by nasel_cc.src_id, nasel_cc.src_type_cd  order by nasel_cc.cc_dttm desc) n
  from nasel_cc    
  --where nasel_cc.src_type_cd = '10'
) cc on cc.src_id = nasel_fa.fa_id and cc.n = 1

where /*nasel_fa_pack.fa_pack_type_cd = '20'
   and*/ nasel_fa_pack.fa_pack_id = p_fa_pack_id
  --and nasel_fa.acct_id in ('0408650000', '1608650000', '1783850000' , '4595850000')
order by nasel_ccb_prem.city, nasel_ccb_prem.address 

) t;
end;


-- ���������� ������ ��������� � ������� �� ��������� 
procedure get_fp_stop_content(p_fa_pack_id in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor)
is
begin
  open rc for 
  select
    rownum
    , 0 CHECK_DATA
    , t.*
  from (
  select 
    --row_number() over() rownum
    nasel_fa.fa_id
    , nasel_fa.acct_id
    , nasel_ccb_prem.ulitsa || nvl2(nasel_ccb_prem.dom, ', �. '  || nasel_ccb_prem.dom,'') || nvl2(nasel_ccb_prem.korp, ', ����. '  || nasel_ccb_prem.korp,'') || nvl2(nasel_ccb_prem.kvartira, ', ��. '  || nasel_ccb_prem.kvartira,'') address
    --, nasel_ccb_prem.address

    , nasel_ccb_prem.city
    , nasel_ccb_prem.fio 
    , nasel_ccb_prem.postal

    , cc.cc_dttm   
    , cc.src_type_cd
    , nasel_ccb_spr.descr spr_descr
    , nasel_ccb_prem.phones
    , trim(substr(nasel_ccb_prem.phones, 1, instr(nasel_ccb_prem.phones,',')-1)) phone
    
    
    , nvl(nasel_fa.end_reg_reading1, 0) end_reg_reading1
    , nvl(nasel_fa.end_reg_reading2, 0) end_reg_reading2

    --, nvl(nasel_ccb_ft_hist.saldo_bt_uch, 0) + nvl(nasel_ccb_ft_hist.saldo_odn_uch, 0) + nvl(nasel_ccb_ft_hist.saldo_a�t_uch, 0) saldo_m3
    , nvl(nasel_fa.saldo_uch, 0) saldo_uch
    , nasel_fa.mtr_serial_nbr 
    , nasel_fa_pack.fa_pack_id 
    , nasel_fa_pack.acct_otdelen 
    , nasel_fa_pack.cre_dttm
    , nasel_ccb_sp.op_area_descr
    , nasel_fa_pack.fa_pack_type_cd
    , dense_rank() over(order by case when nasel_ccb_prem.prnt_prem_type_cd is null then  nasel_ccb_spr.descr else '0' end) grp 
    , (select descr from nasel_lookup_val where nasel_lookup_val.field_name = 'PREM_TYPE_CD' and nasel_lookup_val.field_value = nasel_ccb_prem.prem_type_cd) prem_type_descr
    , fa_char.st_p_dt -- �������� ���� ���������� 
    , nasel_ccb_sp.sa_end_dt
    
    , fa_notice.fa_id notice_fa_id
    , fa_pack_notice.cre_dttm notice_cre_dttm

    --, to_char(nasel_fa_pack.cre_dttm, 'dd.mm.yyyy') cre_dt

    --, nasel_ccb_prem.ulitsa
    --, nasel_ccb_prem.dom
    --, nasel_ccb_prem.korp
    --, nasel_ccb_prem.kvartira
    
  from nasel_fa_pack
  inner join nasel_fa on nasel_fa.fa_pack_id = nasel_fa_pack.fa_pack_id
  inner join nasel_ccb_prem on nasel_ccb_prem.acct_id = nasel_fa.acct_id
  inner join nasel_ccb_sp on nasel_ccb_sp.acct_id = nasel_ccb_prem.acct_id

  --left join nasel_ccb_bseg_read on nasel_ccb_bseg_read.acct_id = nasel_ccb_prem.acct_id 
  --left join nasel_ccb_ft_hist on nasel_ccb_ft_hist.acct_id = nasel_ccb_prem.acct_id and nasel_ccb_ft_hist.calc_dt = trunc(add_months(sysdate,-2), 'mm')
  --inner join nasel_ccb_ft on nasel_ccb_ft.acct_id = nasel_fa.acct_id
    
  left join (
    select nasel_ccb_sa_rel.acct_id
      , nasel_ccb_sa_rel.spr_cd
      , row_number() over (partition by  nasel_ccb_sa_rel.acct_id order by nasel_ccb_sa_rel.sa_rel_type_cd desc) n
    from nasel_ccb_sa_rel 
    --where acct_id in ('0129250000')
  ) sa_rel on sa_rel.acct_id = nasel_ccb_prem.acct_id and sa_rel.n = 1
  left join nasel_ccb_spr on nasel_ccb_spr.spr_cd = sa_rel.spr_cd

  -- ����������� ���� ����������
  /*left join (
     select
       nasel_fa_char.fa_id
       , nasel_fa_char.char_val_dttm st_p_dt 
       , row_number() over(partition by nasel_fa_char.fa_id, nasel_fa_char.char_type_cd order by nasel_fa_char.effdt desc) n
     from nasel_fa_char 
     where trim(char_type_cd) = 'ST-P-DT'
  ) fa_char on fa_char.fa_id = nasel_fa.fa_id and fa_char.n = 1*/

  -- ��������������
  left join (
     select
       fa_id
       , max(decode(trim(char_type_cd), 'ST-P-DT', char_val_dttm)) st_p_dt -- ����������� ���� ����������
       --, max(decode(trim(char_type_cd), 'PRNT-FA', char_val_str)) prnt_fa -- ID �����������
     from (
       select 
         nasel_fa_char.fa_id
         , nasel_fa_char.char_type_cd
         , nasel_fa_char.char_val_dttm
         , nasel_fa_char.char_val_str
         , row_number() over(partition by nasel_fa_char.fa_id, nasel_fa_char.char_type_cd order by nasel_fa_char.effdt desc) n
       from nasel_fa_char 
     ) fa_char
     where fa_char.n = 1
     group by fa_id 
  ) fa_char on fa_char.fa_id = nasel_fa.fa_id 

  -- �����������
  left join nasel_fa fa_notice on fa_notice.fa_id = nasel_fa.prnt_fa_id 
  left join nasel_fa_pack fa_pack_notice on fa_pack_notice.fa_pack_id = fa_notice.fa_pack_id 
    and fa_pack_notice.fa_pack_status_flg not in ('10','20')

  left join nasel_cc cc on cc.src_type_cd = '10' and cc.src_id = fa_notice.fa_id
  /* ��������� ������� � ���������
    2017-05-16 ����������. ����� ���������
  left join (
    select 
       nasel_cc.acct_id
      , nasel_cc.cc_dttm
      , nasel_cc.descr
      , nasel_cc.src_type_cd 
      , row_number() over (partition by nasel_cc.src_id, nasel_cc.src_type_cd  order by nasel_cc.cc_dttm desc) n
    from nasel_cc    
    --where nasel_cc.src_type_cd = '10'
  ) cc on cc.acct_id = nasel_fa.acct_id and cc.n = 1*/

  where nasel_fa_pack.fa_pack_type_cd = '40'
     and nasel_fa_pack.fa_pack_id = p_fa_pack_id
    --and nasel_fa.acct_id in ('0408650000', '1608650000', '1783850000' , '4595850000')

  order by nasel_ccb_prem.city, nasel_ccb_prem.address 

  ) t;
end;



/* ����� �������� �� ���� ����������� */
procedure get_fp_stop_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type
  , p_fa_id in nasel_fa.fa_id%type
  , p_acct_id in nasel_ccb_prem.acct_id%type
  , rc out sys_refcursor)
is
begin
  open rc for 
-- ������ ��� ��������� ������ ��������
-- ������: 2017-02-08
-- �����: vsovchinnikov
--

  select
    rownum
    , 0 CHECK_DATA
    , t.*
  from (
  select 
    nasel_fa_pack.fa_pack_id
    , fa_pack_char.rt_spr_cd rt_spr_cd
    , fa_pack_char.rt_type rt_type
    , fa_pack_char.rt_spr recipient_spr_descr
    , fa_pack_char.rt_addr recipient_address
    , fa_pack_char.rt_post recipient_official_post
    , fa_pack_char.rt_name recipient_official_name
   
    --, padeg_pack.Padeg_FIO(nasel_ccb_spr_l.official_post, '�') official_post
    --, padeg_pack.Padeg_FIO(nasel_ccb_spr_l.official_name, '�') official_name
    
    , nasel_fa_pack.cre_dttm
    , nasel_fa_pack.fa_pack_status_flg
    , fa_char.st_p_dt  -- ����������� ���� ����������
    , fa_char.st_p_dt + 7 st_p_dt_end -- �� ������� ���������� �.�. 2017-06-23
    , (select name1 || substr(name2,1,1) || substr(name3,1,1)  from spr_users where name_u = trim(nasel_fa_pack.user_id)) owner 
 
    , (
      select descr from nasel_lookup_val 
       where nasel_lookup_val.field_name = 'FA_PACK_STATUS_FLG' and nasel_lookup_val.field_value = nasel_fa_pack.fa_pack_status_flg
       ) fa_pack_status_descr
       
    , (
      select nasel_lookup_val.descr
      from nasel_lookup_val
      where nasel_lookup_val.field_name = 'FA_PACK_TYPE_CD' 
        and nasel_lookup_val.field_value = nasel_fa_pack.fa_pack_type_cd
      ) fa_pack_type_descr 
      
    , fa.fa_cnt
      
     /*, case when (p_fa_id is null or (select count(*) from nasel_fa where nasel_fa.fa_pack_id = nasel_fa_pack.fa_pack_id and nasel_fa.fa_id = p_fa_id) > 0) then 1 end fa_id_exists    

     , case when (p_acct_id is null or (select count(*) from nasel_fa where nasel_fa.fa_pack_id = nasel_fa_pack.fa_pack_id and nasel_fa.acct_id = p_acct_id) > 0) then 1 end acct_id_exists*/
   /* , (select nasel_lookup_val.descr from nasel_lookup_val 
       where nasel_lookup_val.field_name = 'PREM_TYPE_CD' and nasel_lookup_val.field_value = nasel_ccb_prem.prem_type_cd) prem_type_descr    
  */
    
    --, padeg_pack.Padeg_FIO('���������������','�')
    --, char_st_p_dt.char_val_dttm st_p_dt -- ����������� ���� ����������
    --, nasel_ccb_spr_l.official_post
    --, nasel_ccb_spr_l.official_name

  from nasel_fa_pack
  
  left join (
    select
      nasel_fa.fa_pack_id
      , count(*) fa_cnt
      , max(case when p_fa_id is null or nasel_fa.fa_id like '%' || p_fa_id || '%' then 1 end) fa_id_exists -- ��� ������ �� fa_id
      , max(case when p_acct_id is null or nasel_fa.acct_id like '%' || p_acct_id || '%' then 1 end) acct_id_exists -- ��� ������ �� acct_id
    from nasel_fa
    --where nasel_fa.fa_id = '0000013975' 
    group by nasel_fa.fa_pack_id
  ) fa on fa.fa_pack_id = nasel_fa_pack.fa_pack_id

  left join (
    select
      fa_pack_id
      , max(case when char_type_cd = 'RT-SPRCD' then char_val_str end) rt_spr_cd
      , max(case when char_type_cd = 'RT-TYPE' then char_val_str end) rt_type
      , max(case when char_type_cd = 'RT-SPR' then char_val_str end) rt_spr
      , max(case when char_type_cd = 'RT-ADDR' then char_val_str end) rt_addr
      , max(case when char_type_cd = 'RT-POST' then char_val_str end) rt_post
      , max(case when char_type_cd = 'RT-NAME' then char_val_str end) rt_name
    from (
      select 
        nasel_fa_pack_char.fa_pack_id
        , trim(nasel_fa_pack_char.char_type_cd) char_type_cd
        , trim(nasel_fa_pack_char.char_val_str) char_val_str
        , row_number() over (partition by nasel_fa_pack_char.fa_pack_id, nasel_fa_pack_char.char_type_cd order by nasel_fa_pack_char.effdt desc) N
      from nasel_fa_pack_char
    )
    where n = 1 
    group by fa_pack_id
  ) fa_pack_char on fa_pack_char.fa_pack_id = nasel_fa_pack.fa_pack_id

  -- ����������� ���� ����������
  -- ���������� ����� ����, ��� 'ST-P-DT' ����� ���������� ��������������� fa
  left join (
    select 
      nasel_fa.fa_pack_id 
      , max(st_p_dt) st_p_dt 
    from (
      select     
        nasel_fa_char.fa_id
        , nasel_fa_char.char_val_dttm st_p_dt 
        , row_number() over(partition by nasel_fa_char.fa_id, nasel_fa_char.char_type_cd order by nasel_fa_char.effdt desc) n
      from nasel_fa_char 
      where trim(char_type_cd) = 'ST-P-DT' 
    ) t
    left join nasel_fa on nasel_fa.fa_id = t.fa_id    
    where t.n = 1
    group by nasel_fa.fa_pack_id     
  ) fa_char on fa_char.fa_pack_id = nasel_fa_pack.fa_pack_id 


  where 
    fa_pack_type_cd = '40' 
    and nasel_fa_pack.fa_pack_status_flg not in ('10','20')
    and nasel_fa_pack.acct_otdelen = p_acct_otdelen 
    and fa.fa_id_exists = 1 and fa.acct_id_exists = 1 -- ������� ��� ������ 
  
    --nasel_fa_pack.fa_pack_id = '0000002160'

  order by nasel_fa_pack.cre_dttm desc

) t
;
end;


/* ���������� ��������������� ������ �������� �� ��������� */
procedure get_pre_stop_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor)  -- get_stop_list
is
begin
  open rc for 
-- ������ ��� ������������� ������ ��������� ��� ����������� ����������
-- �����: �.�.����������
-- ������: 2017-02-01

  select 
    rownum
    , 0 CHECK_DATA  
    , t.* 
    , max(t.grp_data) over() grp_data_max -- ���-�� ������������� �����������
  from (
  select 

    dense_rank() over (order by spr.spr_cd, nvl2(nasel_ccb_prem.prnt_prem_type_cd, 0,1)) grp_data -- ������ ������������� ����������� 
    --dense_rank() over (order by spr.descr) grp_data -- ������ ������������� �����������
    --, count(distinct nvl(spr.descr, ' ')) over() grp_data_max -- ���-�� ������������� �����������

    , nasel_ccb_prem.acct_id
    , fa_notice.approval_dttm
    , fa_notice.cc_dttm
    , nasel_ccb_prem.fio 
    , nasel_ccb_prem.postal 
    , nasel_ccb_prem.city 
    , nasel_ccb_prem.ulitsa || nvl2(nasel_ccb_prem.dom, ', �. '  || nasel_ccb_prem.dom,'') || nvl2(nasel_ccb_prem.korp, ', ����. '  || nasel_ccb_prem.korp,'') || nvl2(nasel_ccb_prem.kvartira, ', ��. '  || nasel_ccb_prem.kvartira,'') address
    , (select nasel_lookup_val.descr from nasel_lookup_val where nasel_lookup_val.field_name = 'PREM_TYPE_CD' and nasel_lookup_val.field_value = nasel_ccb_prem.prem_type_cd) prem_type_descr     
    --, trim(nasel_ccb_prem.acct_otdelen) acct_otdelen
    , ccb_ft.saldo_uch
    , spr.descr spr_descr
    , fa_notice.fa_pack_id
    , (select nasel_lookup_val.descr from nasel_lookup_val where nasel_lookup_val.field_name = 'FA_PACK_TYPE_CD' and nasel_lookup_val.field_value = fa_notice.fa_pack_type_cd) fa_pack_type_descr     
    
    
  from nasel_ccb_prem
  inner join nasel_ccb_sp on nasel_ccb_sp.acct_id = nasel_ccb_prem.acct_id and nasel_ccb_sp.sa_status_flg = '20'

  -- ���������� ��������������� ������
  inner join (
    select
      nasel_ccb_ft_hist.acct_id
      , nvl(nasel_ccb_ft_hist.bill_bt_otch, 0) + nvl(nasel_ccb_ft_hist.bill_odn_otch, 0) bill_otch
    from nasel_ccb_ft_hist
    where nasel_ccb_ft_hist.uch_begin_dt = add_months(trunc(sysdate, 'mm'),-2)
  ) nasel_ccb_ft_hist_m1 on nasel_ccb_ft_hist_m1.acct_id = nasel_ccb_prem.acct_id

  --
  inner join (
    select 
      nasel_ccb_ft.acct_id
      , nasel_ccb_ft.uch_begin_dt
      , nvl(nasel_ccb_ft.bill_bt_otch,0) + nvl(nasel_ccb_ft.bill_odn_otch,0) bill_otch
      , nvl(nasel_ccb_ft.saldo_bt_uch,0) saldo_bt_uch
      , nvl(nasel_ccb_ft.saldo_bt_uch,0) + nvl(nasel_ccb_ft.saldo_odn_uch,0) + nvl(nasel_ccb_ft.saldo_act_uch,0) saldo_uch
    from nasel_ccb_ft
  ) ccb_ft on ccb_ft.acct_id = nasel_ccb_prem.acct_id

  -- ��������� �����������
  inner join (
    select 
      nasel_fa.fa_pack_id
      , nasel_fa_pack.fa_pack_type_cd
      , nasel_fa_pack.cre_dttm
      , nasel_fa_pack.user_id
      , nasel_fa.acct_id
      , nasel_fa.fa_id
      , nasel_cc.approval_dttm
      , nasel_cc.cc_dttm
      , row_number() over (partition by nasel_fa.acct_id order by nasel_fa_pack.cre_dttm desc) N
    from nasel_fa
    inner join nasel_fa_pack on nasel_fa_pack.fa_pack_id = nasel_fa.fa_pack_id 
      and nasel_fa_pack.fa_pack_type_cd in ('10','20') and nasel_fa_pack.fa_pack_status_flg not in ('10','20')
    inner join nasel_cc on nasel_cc.src_id = nasel_fa.fa_id and nasel_cc.src_type_cd = '10' 
      and nasel_cc.approval_dttm is not null      -- and nasel_cc.cc_dttm < sysdate -30 -- ��������������� �� ����������� �.�.�������� 2017-03-23
  ) fa_notice on fa_notice.acct_id = nasel_ccb_prem.acct_id and fa_notice.n = 1

  -- ������ �� ���������� �� ���������� �����������
  left join (
    select nasel_fa.prnt_fa_id
    from nasel_fa
    inner join nasel_fa_pack on nasel_fa_pack.fa_pack_id = nasel_fa.fa_pack_id 
      and nasel_fa_pack.fa_pack_type_cd = '40' 
      --and nasel_fa_pack.fa_pack_status_flg in ('50','60','71','72') 
      and nasel_fa_pack.fa_pack_status_flg not in ('10','20')
    --where nasel_fa.char_type_cd = 'PRNT-FA'
  ) fa_stop on fa_stop.prnt_fa_id = fa_notice.fa_id

  left join (
    select        
      nasel_ccb_sa_rel.acct_id
      , nasel_ccb_spr.descr
      , nasel_ccb_spr.spr_cd
      , row_number() over (partition by nasel_ccb_sa_rel.acct_id order by nasel_ccb_sa_rel.sa_rel_type_cd) n
    from nasel_ccb_sa_rel
    inner join nasel_ccb_spr on nasel_ccb_spr.spr_cd = nasel_ccb_sa_rel.spr_cd
  ) spr on spr.acct_id = fa_notice.acct_id and spr.n = 1

  -- ���������������� ��
  left join nasel_ccb_pp on nasel_ccb_pp.acct_id = nasel_ccb_prem.acct_id and nasel_ccb_pp.pp_stat_flg = '20'

  where
    fa_stop.prnt_fa_id is null -- �� ���������� ����������� �� ���� ������ �� ����������
    and    
    ( 
        (   
           trunc(ccb_ft.uch_begin_dt,'mm') > trunc(fa_notice.cre_dttm,'mm')  
           and
           nvl(ccb_ft.saldo_uch,0) - nvl(nasel_ccb_pp.pp_tot_sched_amt,0) - nvl(ccb_ft.bill_otch, 0) - nvl(nasel_ccb_ft_hist_m1.bill_otch, 0) > 0
           --ccb_ft.saldo_uch - ccb_ft.nach_otch - nvl(nasel_ccb_pp.pp_tot_sched_amt,0) > 0
        ) 
      or 
        ( 
           trunc(ccb_ft.uch_begin_dt,'mm') <= trunc(fa_notice.cre_dttm,'mm')
           and
           nvl(ccb_ft.saldo_uch,0) - nvl(nasel_ccb_pp.pp_tot_sched_amt,0) - nvl(ccb_ft.bill_otch, 0) > 0 
        )
    )
    and
      nasel_ccb_prem.acct_otdelen = p_acct_otdelen
    
    -- and saldo_uch > normativ * 2 and saldo_m3 > normativ * 2
    --and nasel_ccb_prem.acct_id in ('1426650000','5426650000','5526650000','7526650000','9926650000', '0000070000')

  order by nasel_ccb_prem.city, nasel_ccb_prem.address 
  ) t;
end;


/* ���������� ������ �������� �� ������ ������ �� ��������� */
procedure get_fp_cancel_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor)
is
begin
  open rc for 
-- ������ ��� ������ �������� �� ������ ������ �� �����������
-- �����: �.�.����������
-- ������: 2017-04-20

  select
    rownum
    , 0 CHECK_DATA  
    , t.* 
  from
  (
  select
    nasel_fa_pack.fa_pack_id
    , nasel_fa_pack.cre_dttm
    , nasel_fa_pack.fa_pack_status_flg
    , fa.prnt_fa_id
    
    , fa_pack_char.rt_spr   -- ������������ �����������
    , fa_pack_char.rt_addr  -- ����� �����������
    , fa_pack_char.rt_post  -- ��������� 
    , fa_pack_char.rt_name  -- ���
    
    -- ����� ����� �������� ����������� ���� - ������������ ����������� rt_spr � ������������ ������ ��� ������ � �����
    
    /*, nasel_ccb_spr_l.descr rt_spr         -- ������������ �����������
    , nasel_ccb_spr_l.address rt_addr        -- ����� �����������
    , nasel_ccb_spr_l.official_post rt_post  -- ��������� 
    , nasel_ccb_spr_l.official_name rt_name  -- ���
    */
    , (select descr from nasel_lookup_val where field_name = 'FA_PACK_STATUS_FLG' and field_value = nasel_fa_pack.fa_pack_status_flg) fa_pack_status_descr

    , (select count(nasel_fa.acct_id) from nasel_fa where nasel_fa.fa_pack_id = nasel_fa_pack.fa_pack_id) acct_id_cnt

    , (select name1 || substr(name2,1,1) || substr(name3,1,1)  from spr_users where name_u = trim(nasel_fa_pack.user_id)) owner 
  from nasel_fa_pack

  -- ���-�� ��������� � ������ �� ������ ���������� � prnt_fa_id ����� ������
  left join (
    select nasel_fa.fa_pack_id
      , count(*) cnt
      , max(nasel_fa.prnt_fa_id) prnt_fa_id 
      --, max(nasel_fa_char.char_val_str) prnt_fa_id 
    from nasel_fa
    --left join nasel_fa_char on nasel_fa_char.fa_id = nasel_fa.fa_id and nasel_fa_char.char_type_cd = 'PRNT-FA'
    group by nasel_fa.fa_pack_id
  ) fa on fa.fa_pack_id = nasel_fa_pack.fa_pack_id

  -- ������� (��� ����������� �� �������������)
  left join (
    select
      nasel_fa.fa_id
      --, nasel_fa.fa_pack_id
      , nasel_fa_pack_char.char_val_str rt_spr_cd
    from nasel_fa 
    left join nasel_fa_pack_char on nasel_fa_pack_char.fa_pack_id = nasel_fa.fa_pack_id
    where trim(nasel_fa_pack_char.char_type_cd) = 'RT-SPRCD'
  ) prnt_fa on prnt_fa.fa_id = fa.prnt_fa_id

  left join (
    select
      nasel_fa_pack_char.fa_pack_id
      , max(case when trim(nasel_fa_pack_char.char_type_cd) = 'RT-SPR' then nasel_fa_pack_char.char_val_str end) rt_spr
      , max(case when nasel_fa_pack_char.char_type_cd = 'RT-ADDR' then nasel_fa_pack_char.char_val_str end) rt_addr
      , max(case when nasel_fa_pack_char.char_type_cd = 'RT-POST' then nasel_fa_pack_char.char_val_str end) rt_post
      , max(case when nasel_fa_pack_char.char_type_cd = 'RT-NAME' then nasel_fa_pack_char.char_val_str end) rt_name
    from nasel_fa_pack_char
    group by nasel_fa_pack_char.fa_pack_id
  ) fa_pack_char on fa_pack_char.fa_pack_id = nasel_fa_pack.fa_pack_id
  
  /*left join nasel_ccb_spr on trim(nasel_ccb_spr.spr_cd) = trim(prnt_fa.rt_spr_cd)
  left join nasel_ccb_spr_l on nasel_ccb_spr_l.spr_cd = nasel_ccb_spr.spr_cd  */
  
  /*left join (
    select nasel_ccb_sa_rel.acct_id
      , nasel_ccb_sa_rel.spr_cd
      , row_number() over (partition by  nasel_ccb_sa_rel.acct_id order by nasel_ccb_sa_rel.sa_rel_type_cd desc) n
    from nasel_ccb_sa_rel 
  ) sa_rel on sa_rel.acct_id = nasel_ccb_prem.acct_id and sa_rel.n = 1
  left join nasel_ccb_spr on nasel_ccb_spr.spr_cd = sa_rel.spr_cd
  left join nasel_ccb_spr_l on nasel_ccb_spr_l.spr_cd = sa_rel.spr_cd*/

  /*left join (
    select
      nasel_fa.fa_id
      --, nasel_fa.fa_pack_id
      , max(case when trim(nasel_fa_pack_char.char_type_cd) = 'RT-SPR' then nasel_fa_pack_char.char_val_str end) rt_spr
      , max(case when nasel_fa_pack_char.char_type_cd = 'RT-ADDR' then nasel_fa_pack_char.char_val_str end) rt_addr
      , max(case when nasel_fa_pack_char.char_type_cd = 'RT-POST' then nasel_fa_pack_char.char_val_str end) rt_post
      , max(case when nasel_fa_pack_char.char_type_cd = 'RT-NAME' then nasel_fa_pack_char.char_val_str end) rt_name
    from nasel_fa 
    left join nasel_fa_pack_char on nasel_fa_pack_char.fa_pack_id = nasel_fa.fa_pack_id
    group by nasel_fa.fa_id
  ) prnt_fa on prnt_fa.fa_id = fa.prnt_fa_id*/


  where acct_otdelen = p_acct_otdelen --'02-61FL'
    and nasel_fa_pack.fa_pack_type_cd = '45'
    and nasel_fa_pack.fa_pack_status_flg not in ('10')
  ) t;
end;



/* ������ �������� �� ����������� (�������������) */
procedure get_fp_reconnect_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor)
is
begin
  open rc for 
  -- ������ ��� ������ �������� �� ����������� (�������������)
  -- �����: �.�.����������
  -- ������: 2017-05-23

  select
    rownum
    , 0 CHECK_DATA  
    , t.* 
  from
  (
  select
    nasel_fa_pack.fa_pack_id
    , nasel_fa_pack.cre_dttm
    , nasel_fa_pack.fa_pack_status_flg
    , fa.prnt_fa_id
    
    , fa_pack_char.rt_spr   -- ������������ �����������
    , fa_pack_char.rt_addr  -- ����� �����������
    , fa_pack_char.rt_post  -- ��������� 
    , fa_pack_char.rt_name  -- ���
    
    -- ����� ����� �������� ����������� ���� - ������������ ����������� rt_spr � ������������ ������ ��� ������ � �����
    /*, nasel_ccb_spr_l.descr rt_spr         -- ������������ �����������
    , nasel_ccb_spr_l.address rt_addr        -- ����� �����������
    , nasel_ccb_spr_l.official_post rt_post  -- ��������� 
    , nasel_ccb_spr_l.official_name rt_name  -- ���
    */
    , (select descr from nasel_lookup_val where field_name = 'FA_PACK_STATUS_FLG' and field_value = nasel_fa_pack.fa_pack_status_flg) fa_pack_status_descr

    , (select count(nasel_fa.acct_id) from nasel_fa where nasel_fa.fa_pack_id = nasel_fa_pack.fa_pack_id) acct_id_cnt

    , (select name1 || substr(name2,1,1) || substr(name3,1,1)  from spr_users where name_u = trim(nasel_fa_pack.user_id)) owner 
  from nasel_fa_pack

  -- ���-�� ����������� � ������ �� ������������� � prnt_fa_id ����� ����������
  left join (
    select nasel_fa.fa_pack_id
      , count(*) cnt
      , max(nasel_fa.prnt_fa_id) prnt_fa_id 
    from nasel_fa
    --left join nasel_fa_char on nasel_fa_char.fa_id = nasel_fa.fa_id and nasel_fa_char.char_type_cd = 'PRNT-FA'
    group by nasel_fa.fa_pack_id
  ) fa on fa.fa_pack_id = nasel_fa_pack.fa_pack_id

  -- ������� (��� ����������� �� �������������)
  left join (
    select
      nasel_fa.fa_id
      --, nasel_fa.fa_pack_id
      , nasel_fa_pack_char.char_val_str rt_spr_cd
    from nasel_fa 
    left join nasel_fa_pack_char on nasel_fa_pack_char.fa_pack_id = nasel_fa.fa_pack_id
    where trim(nasel_fa_pack_char.char_type_cd) = 'RT-SPRCD'
  ) prnt_fa on prnt_fa.fa_id = fa.prnt_fa_id

  left join (
    select
      nasel_fa_pack_char.fa_pack_id
      , max(case when trim(nasel_fa_pack_char.char_type_cd) = 'RT-SPR' then nasel_fa_pack_char.char_val_str end) rt_spr
      , max(case when nasel_fa_pack_char.char_type_cd = 'RT-ADDR' then nasel_fa_pack_char.char_val_str end) rt_addr
      , max(case when nasel_fa_pack_char.char_type_cd = 'RT-POST' then nasel_fa_pack_char.char_val_str end) rt_post
      , max(case when nasel_fa_pack_char.char_type_cd = 'RT-NAME' then nasel_fa_pack_char.char_val_str end) rt_name
    from nasel_fa_pack_char
    group by nasel_fa_pack_char.fa_pack_id
  ) fa_pack_char on fa_pack_char.fa_pack_id = nasel_fa_pack.fa_pack_id
  
  where acct_otdelen = p_acct_otdelen --'02-61FL'
    and nasel_fa_pack.fa_pack_type_cd = '50'
    and nasel_fa_pack.fa_pack_status_flg not in ('10')
  ) t;
end;


/* ���������� ������ ����������� �� ����������� ������������� �����*/
procedure get_approval_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor)
is
begin
  open rc for 
select
  rownum
  , 0 CHECK_DATA
  , t.*
from (
  select 
    nasel_fa.acct_id  
    , nasel_fa.fa_id
    , nasel_fa.saldo_uch
    , nasel_ccb_prem.ulitsa || nvl2(nasel_ccb_prem.dom, ', �. '  || nasel_ccb_prem.dom,'') || nvl2(nasel_ccb_prem.korp, ', ����. '  || nasel_ccb_prem.korp,'') || nvl2(nasel_ccb_prem.kvartira, ', ��. '  || nasel_ccb_prem.kvartira,'') address
    , nasel_ccb_prem.city
    , initcap(nasel_ccb_prem.fio) fio
    , nasel_ccb_spr.descr service_org
    
    , cc.cc_id
    , cc.cc_dttm   
    , cc.src_type_cd
    , cc.approval_dttm
    , (select name1 || substr(name2,1,1) || substr(name3,1,1)  from spr_users where name_u = trim(cc.approval_user_id)) approval_user 
    , row_number() over (partition by nasel_fa.acct_id order by nasel_fa_pack.cre_dttm desc) n
  from nasel_fa 
  inner join nasel_fa_pack on nasel_fa_pack.fa_pack_id = nasel_fa.fa_pack_id
  
  left join (
    select nasel_ccb_sa_rel.acct_id
      , nasel_ccb_sa_rel.spr_cd
      , row_number() over (partition by  nasel_ccb_sa_rel.acct_id order by nasel_ccb_sa_rel.sa_rel_type_cd desc) n
    from nasel_ccb_sa_rel 
    --where acct_id in ('0129250000')
  ) sa_rel on sa_rel.acct_id = nasel_fa.acct_id and sa_rel.n = 1
  left join nasel_ccb_spr on nasel_ccb_spr.spr_cd = sa_rel.spr_cd
                                                          
  inner join (
    select 
      nasel_cc.acct_id
      , nasel_cc.cc_id
      , nasel_cc.cc_dttm
      , nasel_cc.descr
      , nasel_cc.src_type_cd 
      , nasel_cc.approval_user_id
      , nasel_cc.approval_dttm
      , row_number() over (partition by nasel_cc.src_id, nasel_cc.src_type_cd  order by nasel_cc.cre_dttm desc) n
    from nasel_cc    
    where nasel_cc.src_type_cd = '10' and nasel_cc.cc_status_flg = '20'
  ) cc on cc.acct_id = nasel_fa.acct_id and cc.n = 1
  
  left join nasel_ccb_prem on nasel_ccb_prem.acct_id = nasel_fa.acct_id
  where nasel_fa_pack.acct_otdelen = p_acct_otdelen
    and nasel_fa_pack.fa_pack_status_flg not in ('10','20')

  order by nasel_ccb_prem.city, nasel_ccb_prem.address 

) t where t.n = 1;
end;






/* ���������� ��������������� ������ �� ����� */
procedure get_pre_post_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor)
is
begin
  open rc for 
-- ������ �� �����
-- 2017-02-22
-- �����: ������������
-- �.�.�������
--
-- �������
-- 1. ����� ����� � ������� ����������� (20)
-- 2. saldo_uch > normativ * 2 and saldo_m3 > normativ * 2
-- 3. ���� ���������� ������������� �������� ����� 6 ������� ��� ������
-- 4. ���� ���������� ����������� ����� 2 ������� ��� ������

/*
select
  rownum 
  , 0 CHECK_DATA 
  , acct_id  
  , initcap(fio) fio   
  , city
  , address 
  , saldo_uch 
  , saldo_m3 
  , cc_dttm  
  , fa_id
  , fa_pack_id
  , cre_dttm 
  , acct_otdelen
  , service_org

  , op_area_descr
   
*/

select
  rownum 
  , 0 CHECK_DATA 
  , t.*
from (

  select nasel_ccb_prem.acct_id
    , last_fa.cre_dttm
    , last_fa.fa_pack_id
    , nasel_ccb_ft.saldo_bt_uch
    , nasel_ccb_ft.saldo_act_uch     
    
    , initcap(nasel_ccb_prem.fio) fio   
    , nasel_ccb_prem.city
    , nasel_ccb_prem.ulitsa || nvl2(nasel_ccb_prem.dom, ', �. '  || nasel_ccb_prem.dom,'') || nvl2(nasel_ccb_prem.korp, ', ����. '  || nasel_ccb_prem.korp,'') || nvl2(nasel_ccb_prem.kvartira, ', ��. '  || nasel_ccb_prem.kvartira,'') address


    , nvl(nasel_ccb_ft.saldo_bt_uch, 0) + nvl(nasel_ccb_ft.saldo_odn_uch, 0) + nvl(nasel_ccb_ft.saldo_act_uch, 0) saldo_uch
    , nvl(nasel_ccb_ft_hist.saldo_bt_uch, 0) + nvl(nasel_ccb_ft_hist.saldo_odn_uch, 0) + nvl(nasel_ccb_ft_hist.saldo_act_uch, 0) saldo_m3
                              
    --, pk_nasel_otdel.get_normativ(nasel_ccb_prem.acct_id) normativ
      
    , nasel_ccb_spr.descr service_org
    , nasel_ccb_sp.op_area_descr
    , nasel_ccb_sp.fl_tar11
    , nasel_calc.norm_amt
       
  from nasel_ccb_prem

  -- ��������� �����������
  inner join (
    select nasel_fa.acct_id 
      , nasel_fa.fa_id
      , nasel_fa_pack.cre_dttm
      , nasel_fa_pack.fa_pack_id
      --fa_pack_id
      , row_number() over (partition by nasel_fa.acct_id order by nasel_fa_pack.cre_dttm desc) n
    from nasel_fa           
    inner join nasel_fa_pack on nasel_fa_pack.fa_pack_id = nasel_fa.fa_pack_id 
      and nasel_fa_pack.fa_pack_status_flg not in ('10','20')
      and nasel_fa_pack.fa_pack_type_cd in ('10','20') 
  ) last_fa on last_fa.acct_id = nasel_ccb_prem.acct_id and last_fa.n = 1 and trunc(last_fa.cre_dttm, 'dd') < trunc(sysdate, 'dd') - 2   

  /*inner join nasel_fa on nasel_fa.acct_id = nasel_ccb_prem.acct_id 
  inner join nasel_fa_pack on nasel_fa_pack.fa_pack_id = nasel_fa.fa_pack_id
    and nasel_fa_pack.cre_dttm < sysdate - 0 */  
                             
  left join nasel_cc on nasel_cc.src_id = last_fa.fa_id and nasel_cc.src_type_cd = '10'

  inner join nasel_ccb_sp on nasel_ccb_sp.acct_id = nasel_ccb_prem.acct_id and nasel_ccb_sp.sa_status_flg in ('20')
  left join nasel_ccb_ft on nasel_ccb_ft.acct_id = nasel_ccb_prem.acct_id
  left join nasel_ccb_ft_hist on nasel_ccb_ft_hist.acct_id = nasel_ccb_prem.acct_id 
    and nasel_ccb_ft_hist.uch_begin_dt = add_months((select begin_dt from nasel_ccb_cal),-3)
    --and nasel_ccb_ft_hist.uch_begin_dt = trunc(add_months((select char_val_dttm from cfg_task where char_type_cd = 'REP_DTTM' and task_name = 'P_UPDATE_NASEL_JOB_DAY'),-3), 'mm') -- 2017-05-31
  
             
  left join (
    select nasel_ccb_sa_rel.acct_id
      , nasel_ccb_sa_rel.spr_cd
      , row_number() over (partition by  nasel_ccb_sa_rel.acct_id order by nasel_ccb_sa_rel.sa_rel_type_cd desc) n
    from nasel_ccb_sa_rel 
  ) sa_rel on sa_rel.acct_id = nasel_ccb_prem.acct_id and sa_rel.n = 1
  left join nasel_ccb_spr on nasel_ccb_spr.spr_cd = sa_rel.spr_cd
  
  inner join nasel_calc on nasel_calc.acct_id = nasel_ccb_prem.acct_id  

  where nasel_cc.cc_id is null 
    and acct_otdelen = p_acct_otdelen
    /*and acct_id in ('4015250000', '4046250000', '6046250000','1441170000','0000003304', '7474750000','7204750000','4006750000','6333850000','1390950000', '6326650000', '3043850000', '9226650000', 
      '0408650000', '1608650000', '1783850000' , '4595850000') /**/
  
) t where saldo_uch > norm_amt * 2 * fl_tar11 and saldo_m3 > norm_amt * 2 * fl_tar11;
    --and (cc_dttm is null or months_between(cc_dttm, sysdate) > 6) 
    --and (cre_dttm is null or months_between(cre_dttm, sysdate) > 2)
end;


-- ���������� ������ �������� �����������
procedure get_fp_notices_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type
  , p_fa_id in nasel_fa.fa_id%type
  , p_acct_id in nasel_ccb_prem.acct_id%type
  , rc out sys_refcursor)
is
begin
  open rc for 
  select
    rownum
    , 0 CHECK_DATA 
    , t.*
   from (
    select
      nasel_fa_pack.*
      , fa.fa_cnt
      , fa.fa_cnt cnt -- ������
      , (
        select descr 
        from nasel_lookup_val 
        where field_name= 'FA_PACK_STATUS_FLG' 
          and field_value= nasel_fa_pack.fa_pack_status_flg
        ) fa_pack_status_descr
      , (
        select nasel_lookup_val.descr
        from nasel_lookup_val
        where nasel_lookup_val.field_name = 'FA_PACK_TYPE_CD' 
          and nasel_lookup_val.field_value = nasel_fa_pack.fa_pack_type_cd
      ) fa_pack_type_descr     

    , (select name1 || substr(name2,1,1) || substr(name3,1,1)  from spr_users where name_u = trim(nasel_fa_pack.user_id)) owner 
    from nasel_fa_pack
    
    left join (
      select
        nasel_fa.fa_pack_id
        , count(*) fa_cnt
        , max(case when p_fa_id is null or nasel_fa.fa_id like '%' || p_fa_id || '%' then 1 end) fa_id_exists -- ��� ������ �� fa_id
        , max(case when p_acct_id is null or nasel_fa.acct_id like '%' || p_acct_id || '%' then 1 end) acct_id_exists -- ��� ������ �� acct_id
      from nasel_fa
      group by nasel_fa.fa_pack_id
    ) fa on fa.fa_pack_id = nasel_fa_pack.fa_pack_id

    where nasel_fa_pack.acct_otdelen = p_acct_otdelen
      and nasel_fa_pack.fa_pack_type_cd in ('10','20') 
      and nasel_fa_pack.fa_pack_status_flg not in ('10','20')
      and fa.fa_id_exists = 1 and acct_id_exists = 1 -- ������� ��� ������

    order by nasel_fa_pack.cre_dttm desc 
  ) t;
end;


/* ����� �������� �� ������ ����������� �� ������� */
procedure get_fp_cancel_stop_content(p_fa_pack_id in nasel_fa_pack.fa_pack_id%type, rc out sys_refcursor)
is
begin
  open rc for 
  select 
    nasel_ccb_prem.acct_id
    , nasel_ccb_prem.fio
    , fa_stop.fa_id stop_fa_id
    , fa_stop.fa_pack_id stop_fa_pack_id
    , fa_stop.saldo_uch stop_saldo_uch
    , fa_pack_stop.cre_dttm stop_cre_dttm
    
    , fa_notice.fa_id notice_fa_id
    , fa_notice.fa_pack_id notice_fa_pack_id
    , fa_pack_notice.cre_dttm notice_cre_dttm
    , fa_char_notice.st_p_dt

    , nasel_ccb_prem.city
    , nasel_ccb_prem.ulitsa || nvl2(nasel_ccb_prem.dom, ', �. '  || nasel_ccb_prem.dom,'') || nvl2(nasel_ccb_prem.korp, ', ����. '  || nasel_ccb_prem.korp,'') || nvl2(nasel_ccb_prem.kvartira, ', ��. '  || nasel_ccb_prem.kvartira,'') address
    , (select descr from nasel_lookup_val where field_name = 'PREM_TYPE_CD' and field_value = nasel_ccb_prem.prem_type_cd) prem_type_descr
    
    /*, nvl(nasel_ccb_spr_l.descr, nasel_ccb_spr.descr) rt_spr1   -- ������������ �����������
    , nasel_ccb_spr_l.address rt_addr  -- ����� �����������
    , nasel_ccb_spr_l.official_post rt_post  -- ��������� 
    , nasel_ccb_spr_l.official_name rt_name   -- ���*/

     
  -- ������ �� ������ �����������
  from nasel_fa_pack
  left join nasel_fa on nasel_fa.fa_pack_id = nasel_fa_pack.fa_pack_id
  
  /*left join (
    select nasel_fa_char.fa_id
      , nasel_fa_char.char_val_str prnt_fa
    from nasel_fa_char
    where nasel_fa_char.char_type_cd = 'PRNT-FA'
  ) fa_char on fa_char.fa_id = nasel_fa.fa_id*/ 
  
  -- ������ �� �����������  
  left join nasel_fa fa_stop on fa_stop.fa_id = nasel_fa.prnt_fa_id
  left join nasel_fa_pack fa_pack_stop on fa_pack_stop.fa_pack_id = fa_stop.fa_pack_id
  /*left join (
    select nasel_fa_char.fa_id
      , nasel_fa_char.char_val_str prnt_fa
    from nasel_fa_char
    where nasel_fa_char.char_type_cd = 'PRNT-FA'
  ) fa_char_stop on fa_char_stop.fa_id = fa_stop.fa_id */
  
  -- �����������
  left join nasel_fa fa_notice on fa_notice.fa_id = fa_stop.prnt_fa_id
  left join nasel_fa_pack fa_pack_notice on fa_pack_notice.fa_pack_id = fa_notice.fa_pack_id
  left join (
    select nasel_fa_char.fa_id
      , nasel_fa_char.char_val_dttm st_p_dt
    from nasel_fa_char
    where nasel_fa_char.char_type_cd = 'ST-P-DT'
  ) fa_char_notice on fa_char_notice.fa_id = fa_stop.fa_id 

  -- ������ ������������
  inner join nasel_ccb_prem on nasel_ccb_prem.acct_id = nasel_fa.acct_id
  
  /*-- �������
  left join (
    select nasel_ccb_sa_rel.acct_id
      , nasel_ccb_sa_rel.spr_cd
      , row_number() over (partition by  nasel_ccb_sa_rel.acct_id order by nasel_ccb_sa_rel.sa_rel_type_cd desc) n
    from nasel_ccb_sa_rel 
  ) sa_rel on sa_rel.acct_id = nasel_ccb_prem.acct_id and sa_rel.n = 1
  left join nasel_ccb_spr on nasel_ccb_spr.spr_cd = sa_rel.spr_cd
  left join nasel_ccb_spr_l on nasel_ccb_spr_l.spr_cd = sa_rel.spr_cd*/
  
  where nasel_fa_pack.fa_pack_type_cd = '45' 
    --and nasel_fa_pack.fa_pack_status_flg not in ('10','20')
    and nasel_fa_pack.fa_pack_id = p_fa_pack_id
  order by nasel_fa_pack.cre_dttm desc;
end;

/* ����� �������� �� ������������� �� ������� 
  */
procedure get_fp_reconnect_content(p_fa_pack_id in nasel_fa_pack.fa_pack_id%type, rc out sys_refcursor)
is
begin
  open rc for 
  select 
    nasel_ccb_prem.acct_id
    , nasel_ccb_prem.fio
    , fa_stop.fa_id stop_fa_id
    , fa_stop.fa_pack_id stop_fa_pack_id
    , fa_pack_stop.cre_dttm stop_cre_dttm
    
    , fa_notice.fa_id notice_fa_id
    , fa_notice.fa_pack_id notice_fa_pack_id
    , fa_pack_notice.cre_dttm notice_cre_dttm
    , fa_char_notice.st_p_dt

    , nasel_ccb_prem.phones
    , nasel_ccb_prem.city
    , nasel_ccb_prem.ulitsa || nvl2(nasel_ccb_prem.dom, ', �. '  || nasel_ccb_prem.dom,'') || nvl2(nasel_ccb_prem.korp, ', ����. '  || nasel_ccb_prem.korp,'') || nvl2(nasel_ccb_prem.kvartira, ', ��. '  || nasel_ccb_prem.kvartira,'') address
    , (select descr from nasel_lookup_val where field_name = 'PREM_TYPE_CD' and field_value = nasel_ccb_prem.prem_type_cd) prem_type_descr
    
    /*, nvl(nasel_ccb_spr_l.descr, nasel_ccb_spr.descr) rt_spr1   -- ������������ �����������
    , nasel_ccb_spr_l.address rt_addr  -- ����� �����������
    , nasel_ccb_spr_l.official_post rt_post  -- ��������� 
    , nasel_ccb_spr_l.official_name rt_name   -- ���*/

     
  -- ������ �� ������ �����������
  from nasel_fa_pack
  left join nasel_fa on nasel_fa.fa_pack_id = nasel_fa_pack.fa_pack_id
  
  /*left join (
    select nasel_fa_char.fa_id
      , nasel_fa_char.char_val_str prnt_fa
    from nasel_fa_char
    where nasel_fa_char.char_type_cd = 'PRNT-FA'
  ) fa_char on fa_char.fa_id = nasel_fa.fa_id*/ 
  
  -- ������ �� �����������  
  left join nasel_fa fa_stop on fa_stop.fa_id = nasel_fa.prnt_fa_id
  left join nasel_fa_pack fa_pack_stop on fa_pack_stop.fa_pack_id = fa_stop.fa_pack_id
  /*left join (
    select nasel_fa_char.fa_id
      , nasel_fa_char.char_val_str prnt_fa
    from nasel_fa_char
    where nasel_fa_char.char_type_cd = 'PRNT-FA'
  ) fa_char_stop on fa_char_stop.fa_id = fa_stop.fa_id */
  
  -- �����������
  left join nasel_fa fa_notice on fa_notice.fa_id = fa_stop.prnt_fa_id
  left join nasel_fa_pack fa_pack_notice on fa_pack_notice.fa_pack_id = fa_notice.fa_pack_id
  left join (
    select nasel_fa_char.fa_id
      , nasel_fa_char.char_val_dttm st_p_dt
    from nasel_fa_char
    where nasel_fa_char.char_type_cd = 'ST-P-DT'
  ) fa_char_notice on fa_char_notice.fa_id = fa_stop.fa_id 

  -- ������ ������������
  inner join nasel_ccb_prem on nasel_ccb_prem.acct_id = nasel_fa.acct_id
  
 
  where nasel_fa_pack.fa_pack_type_cd = '50' 
    --and nasel_fa_pack.fa_pack_status_flg not in ('10','20')
    and nasel_fa_pack.fa_pack_id = p_fa_pack_id
  order by nasel_fa_pack.cre_dttm desc;
end;


/* ����� �������� �� ������ ����������� �� ������� */
procedure get_fa_reconnect_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor)
is
begin
  open rc for 
  select 
    nasel_ccb_prem.acct_id
    , nasel_ccb_prem.fio
    , fa_stop.fa_id stop_fa_id
    , fa_stop.fa_pack_id stop_fa_pack_id
    , fa_pack_stop.cre_dttm stop_cre_dttm
    
    , fa_notice.fa_id notice_fa_id
    , fa_notice.fa_pack_id notice_fa_pack_id
    , fa_pack_notice.cre_dttm notice_cre_dttm
    , fa_char_notice.st_p_dt

    , nasel_ccb_prem.city
    , nasel_ccb_prem.ulitsa || nvl2(nasel_ccb_prem.dom, ', �. '  || nasel_ccb_prem.dom,'') || nvl2(nasel_ccb_prem.korp, ', ����. '  || nasel_ccb_prem.korp,'') || nvl2(nasel_ccb_prem.kvartira, ', ��. '  || nasel_ccb_prem.kvartira,'') address
    , (select descr from nasel_lookup_val where field_name = 'PREM_TYPE_CD' and field_value = nasel_ccb_prem.prem_type_cd) prem_type_descr
    
    /*, nvl(nasel_ccb_spr_l.descr, nasel_ccb_spr.descr) rt_spr1   -- ������������ �����������
    , nasel_ccb_spr_l.address rt_addr  -- ����� �����������
    , nasel_ccb_spr_l.official_post rt_post  -- ��������� 
    , nasel_ccb_spr_l.official_name rt_name   -- ���*/

     
  -- ������ �� ������ �����������
  from nasel_fa_pack
  left join nasel_fa on nasel_fa.fa_pack_id = nasel_fa_pack.fa_pack_id
  
  /*left join (
    select nasel_fa_char.fa_id
      , nasel_fa_char.char_val_str prnt_fa
    from nasel_fa_char
    where nasel_fa_char.char_type_cd = 'PRNT-FA'
  ) fa_char on fa_char.fa_id = nasel_fa.fa_id*/ 
  
  -- ������ �� �����������  
  left join nasel_fa fa_stop on fa_stop.fa_id = nasel_fa.prnt_fa_id
  left join nasel_fa_pack fa_pack_stop on fa_pack_stop.fa_pack_id = fa_stop.fa_pack_id
    and fa_pack_stop.fa_pack_status_flg not in ('10','20')
  
  /*left join (
    select nasel_fa_char.fa_id
      , nasel_fa_char.char_val_str prnt_fa
    from nasel_fa_char
    where nasel_fa_char.char_type_cd = 'PRNT-FA'
  ) fa_char_stop on fa_char_stop.fa_id = fa_stop.fa_id*/ 
  
  -- �����������
  left join nasel_fa fa_notice on fa_notice.fa_id = fa_stop.prnt_fa_id
  left join nasel_fa_pack fa_pack_notice on fa_pack_notice.fa_pack_id = fa_notice.fa_pack_id 
    and fa_pack_notice.fa_pack_status_flg not in ('10','20')
  left join (
    select nasel_fa_char.fa_id
      , nasel_fa_char.char_val_dttm st_p_dt
    from nasel_fa_char
    where nasel_fa_char.char_type_cd = 'ST-P-DT'
  ) fa_char_notice on fa_char_notice.fa_id = fa_stop.fa_id 

  -- ������ ������������
  inner join nasel_ccb_prem on nasel_ccb_prem.acct_id = nasel_fa.acct_id
  
  /*-- �������
  left join (
    select nasel_ccb_sa_rel.acct_id
      , nasel_ccb_sa_rel.spr_cd
      , row_number() over (partition by  nasel_ccb_sa_rel.acct_id order by nasel_ccb_sa_rel.sa_rel_type_cd desc) n
    from nasel_ccb_sa_rel 
  ) sa_rel on sa_rel.acct_id = nasel_ccb_prem.acct_id and sa_rel.n = 1
  left join nasel_ccb_spr on nasel_ccb_spr.spr_cd = sa_rel.spr_cd
  left join nasel_ccb_spr_l on nasel_ccb_spr_l.spr_cd = sa_rel.spr_cd*/
  
  where acct_otdelen = p_acct_otdelen --'02-61FL'
    and nasel_fa_pack.fa_pack_type_cd = '50'
    and nasel_fa_pack.fa_pack_status_flg not in ('10');

end;


/*
procedure get_fa_pack_stop_list(p_acct_otdelen in nasel_ccb_prem.acct_otdelen%type, rc out sys_refcursor)
is
begin
  open rc for 
select
  rownum
  , 0 CHECK_DATA 
  , t.cre_dttm
  , t.acct_otdelen
  , t.fa_pack_id
  , t.fa_pack_type_cd
  --, decode(trim(t.fa_pack_type_cd),'20','�����������', '40', '�����������') fa_pack_type_descr
  , t.cnt fa_cnt
  , t.fa_pack_type_descr
  , (select descr from nasel_lookup_val where field_name= 'FA_PACK_STATUS_FLG' and field_value=t.fa_pack_status_flg) fa_pack_status_descr
from (
  select
    nasel_fa_pack.*
    , fa.cnt
    , nasel_lookup_val.descr fa_pack_type_descr
  from nasel_fa_pack
  
  left join (
    select
      nasel_fa.fa_pack_id
      , count(*) cnt
    from nasel_fa
    group by nasel_fa.fa_pack_id
  ) fa on fa.fa_pack_id = nasel_fa_pack.fa_pack_id

  left join nasel_lookup_val on nasel_lookup_val.field_name = 'FA_PACK_TYPE_CD' 
    and nasel_lookup_val.field_value = nasel_fa_pack.fa_pack_type_cd
 

  where nasel_fa_pack.acct_otdelen = p_acct_otdelen
    and ( ( 0 = :app_mode and fa_pack_type_cd in ('10','20') ) 
     or (:app_mode = 1 and fa_pack_type_cd in ('40') ) )
    --and trim(fa_pack_type_cd) = :fa_pack_type_cd

  order by nasel_fa_pack.cre_dttm desc 
) t;
end;*/



 
begin
  null;
end pk_nasel_sweety;



  /*open p_cursor;
  loop
    fetch p_cursor into v_res;
    exit when p_cursor%notfound;
    DBMS_OUTPUT.put_line(v_res.acct_id);
  end loop;
  close p_cursor; */ 
 
  /*for r in p_cursor
  loop
    null;
  end loop;*/
/
