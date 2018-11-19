SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
CREATE SCHEMA extra;
ALTER SCHEMA extra OWNER TO ncuser;
CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;
COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';
CREATE EXTENSION IF NOT EXISTS adminpack WITH SCHEMA pg_catalog;
COMMENT ON EXTENSION adminpack IS 'administrative functions for PostgreSQL';
CREATE EXTENSION IF NOT EXISTS hstore WITH SCHEMA public;
COMMENT ON EXTENSION hstore IS 'data type for storing sets of (key, value) pairs';
CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;
COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';
SET search_path = extra, pg_catalog;
CREATE FUNCTION updatebatch(batchid character varying) RETURNS void
    LANGUAGE plpgsql
    AS $_$
DECLARE
	
BEGIN
	UPDATE extra.im_jobbatch SET errors=rec.failed , success=rec.success 
		FROM 
			(SELECT COUNT(DISTINCT CASE WHEN ERROR = true THEN rowno ELSE null END) AS "failed", COUNT(DISTINCT CASE WHEN ERROR = false THEN rowno ELSE null END) AS "success" 
				FROM extra.im_jobreport WHERE batch=$1 and jobrecord = true) 
			AS rec 
		WHERE id= $1;
END;
$_$;
ALTER FUNCTION extra.updatebatch(batchid character varying) OWNER TO ncuser;
CREATE FUNCTION updatebatchtrigger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
	batchId VARCHAR;
	rec RECORD;
BEGIN
	IF (TG_OP = 'INSERT') THEN
		batchId := NEW.batch;
		PERFORM extra.updateBatch(batchId);
		RETURN NULL; 
	END IF;
END;
$$;
ALTER FUNCTION extra.updatebatchtrigger() OWNER TO ncuser;
SET search_path = public, pg_catalog;
CREATE FUNCTION ad_update_search_pharase() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  country_code varchar;
  country_name varchar;
  place_name varchar;
  place_alnm varchar;
BEGIN
	SELECT cocd, "DESC" INTO country_code, country_name  FROM co WHERE co.coid = NEW.coid;
	SELECT "DESC", altn INTO place_name, place_alnm  FROM pl WHERE pl.plid = NEW.plid;
	
	NEW.cocd :=  country_code;
	NEW.countryName :=  country_name;
	NEW.plcd :=  place_name;
	NEW.placeAltName :=  place_alnm;
	
	NEW.search_phrase :=
	    setweight(to_tsvector(coalesce(new.cnam,'')), 'A') ||
	    setweight(to_tsvector(coalesce(new.adr1,'')), 'B') ||
	    setweight(to_tsvector(coalesce(new.adr2,'')), 'C') ||
	    setweight(to_tsvector(coalesce(new.adr3,'')), 'D') ||
	    setweight(to_tsvector(coalesce(new.adr4,'')), 'A') ||
	    setweight(to_tsvector(coalesce(new.zpcd,'')), 'A') ||
	    setweight(to_tsvector(coalesce(new.phon,'')), 'C') ||
	    setweight(to_tsvector(coalesce(new.emal,'')), 'C') ||
	    setweight(to_tsvector(coalesce(new.ctnm,'')), 'A') ||
	    setweight(to_tsvector(coalesce(place_name,'')), 'C') ||
	    setweight(to_tsvector(coalesce(place_alnm,'')), 'C') ||
	    setweight(to_tsvector(coalesce(country_code,'')), 'C') ||
	    setweight(to_tsvector(coalesce(country_name,'')), 'C');	  
	RETURN NEW;
END;
$$;
ALTER FUNCTION public.ad_update_search_pharase() OWNER TO ncuser;
CREATE FUNCTION ci_patch_supplier_xxx_id() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
	 supplierId varchar;
BEGIN
	IF COALESCE(NEW.suid, '') = 'XXX' AND NEW.lgid IS NOT NULL THEN		
		SELECT suid INTO supplierId FROM lg WHERE lg.lgid = NEW.lgid;
		NEW.suid = supplierId;
	END IF;
	
RETURN NEW;
END;
$$;
ALTER FUNCTION public.ci_patch_supplier_xxx_id() OWNER TO ncuser;
CREATE FUNCTION create_custom_series(name character varying, next_number bigint, size integer) RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
	seq_name character varying;
	seq_relname character varying;	
	schma_name character varying;
	max_val bigint;
	min_val bigint;
	curr_val bigint;
BEGIN
	seq_name := lower(name);
	SELECT INTO schma_name CASE WHEN POSITION('.' in seq_name) > 0 THEN SUBSTRING( seq_name, 0, POSITION('.' in seq_name) ) ELSE  'public' END ;
	SELECT INTO seq_name CASE WHEN POSITION('.' in seq_name) > 0 THEN SUBSTRING( seq_name,POSITION('.' in seq_name) + 1 ) ELSE  seq_name END ;
	--RAISE EXCEPTION 'Seq %', seq_name;
	SELECT INTO seq_relname  c.relname FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace WHERE  c.relname = seq_name AND n.nspname= schma_name AND  c.relkind = 'S';
	
	IF seq_relname IS NULL THEN	-- New
		max_val := get_max_number(size);
		min_val := get_min_number(size);
		EXECUTE 'CREATE SEQUENCE '|| schma_name || '.' || seq_name|| ' INCREMENT 1 MINVALUE 1 MAXVALUE ' || max_val || ' START ' || next_number || ' ;';
		RAISE NOTICE 'SEQUENCE % CREATED', seq_name;		
	ELSE -- Exist	
		RAISE NOTICE 'SEQUENCE % ALREADY EXIST', name;
		PERFORM NEXTVAL( name );
		SELECT INTO curr_val CURRVAL( name );	 
		IF curr_val + 1 < next_number THEN	 
				 max_val := get_max_number(size);	 
				 min_val := get_min_number(size);	 				 
				 EXECUTE 'DROP SEQUENCE IF EXISTS ' || seq_name || ';';	 
				 EXECUTE 'CREATE SEQUENCE '|| schma_name || '.' || seq_name|| ' INCREMENT 1 MINVALUE 1 MAXVALUE ' || max_val || ' START ' || next_number || ' ;';
				 RAISE NOTICE 'SEQUENCE % UPDATED AS NEXT VALUE CHANGED', seq_name;	 
				 
		 END IF;
		
	END IF;
END;
$$;
ALTER FUNCTION public.create_custom_series(name character varying, next_number bigint, size integer) OWNER TO ncuser;
CREATE FUNCTION cs_invoice_key_update() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE  
	delay int;
	diff interval;
	expected timestamp with time zone;
	delivered timestamp with time zone;
BEGIN
	-- Invoice key update
	NEW.ikey := CASE WHEN NEW.inok = 'Y' THEN 'I' WHEN NEW.riok = 'Y' THEN 'R' ELSE 'C' END;
	
	-- Updating initial sales price during booking
	if (NEW.qtok = 'Y' OR NEW.bkok = 'Y') AND COALESCE(NEW.fslp, 0.00) = 0.00 THEN
		NEW.fslp = NEW.qprc;
	END IF;
	
	SET TIME ZONE 'UTC';
	
	-- delay calculation
	If NEW.dday IS NOT NULL AND NEW.dtim IS NOT NULL THEN
		IF NEW.podt IS NULL THEN
			delivered = now() ;
		ELSE
			IF NEW.potm IS NULL THEN 
				delivered = NEW.podt; 
			ELSE
				delivered = to_char(NEW.podt,'YYYY-MM-DD') || ' ' || NEW.potm; 
			END IF;
		END IF;
		expected = to_char(NEW.dday,'YYYY-MM-DD') || ' ' || NEW.dtim; 
		diff = age(delivered, expected);
		--RAISE NOTICE 'Expected: % ; Delivered: % ;Age: % ; Mins: %', expected, delivered, diff, round(EXTRACT(epoch FROM diff)/60);
		NEW.dlay = round(EXTRACT(epoch FROM diff)/60);
		
	END IF;
	
	-- DELETE KEY
	If NEW.stat = 'D' THEN 
		NEW.skey = 'DELETE';
		RETURN NEW;
	END IF;
	-- not booked, quoted
	IF COALESCE(NEW.bkok,'N') = 'N' THEN
		NEW.skey = '0000';
		RETURN NEW;
	END IF;
	
	-- delivered
	IF COALESCE(NEW.dlok,'N') = 'Y' THEN
		NEW.skey = 'DY';
		RETURN NEW;
	END IF;
	
	-- manifested
	IF COALESCE(NEW.mfok,'N') = 'Y' THEN
		NEW.skey = 'MYDN';
		RETURN NEW;
	END IF;
	
	-- processing
	IF COALESCE(NEW.rcok,'N') = 'Y' THEN
		NEW.skey = 'RYMN';
		RETURN NEW;
	END IF;
	
	-- parcel on board
	IF COALESCE(NEW.clok,'N') = 'Y' THEN
		NEW.skey = 'CYRN';
		RETURN NEW;
	END IF;
	
	-- driver assinged/Awaiting collection
	IF COALESCE(NEW.dvok,'N') = 'Y' THEN
		NEW.skey = 'CNRNAY';
		RETURN NEW;
	END IF;
	
	-- Just booked/Assign adiver queue
	IF COALESCE(NEW.bkok,'N') = 'Y' THEN
		NEW.skey = 'BYANRN';
		RETURN NEW;
	END IF;
	
	
	RETURN NEW;
END;
$$;
ALTER FUNCTION public.cs_invoice_key_update() OWNER TO ncuser;
CREATE FUNCTION cs_update_summery() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE  	
	client RECORD;
	credit_used  NUMERIC;
	consignmentId CHARACTER VARYING;
	sales RECORD;
	costs RECORD;
	leg RECORD;
	tleg RECORD;
	manifest RECORD;
	
	colAd RECORD; 
	delAd RECORD; 
	
	cv_tpcl CHARACTER VARYING;
	cv_ocol CHARACTER VARYING;
	cv_odel CHARACTER VARYING;
	cv_coag CHARACTER VARYING;
	cv_ftls CHARACTER VARYING;
	cv_ftln CHARACTER VARYING;
	cv_ftdt CHARACTER VARYING;
	cv_ftat CHARACTER VARYING;
	cv_ftma CHARACTER VARYING;
	cv_deag CHARACTER VARYING;
	cv_fdln CHARACTER VARYING;
	cv_fddt  CHARACTER VARYING;
	cv_fdat CHARACTER VARYING;
	cv_spne NUMERIC(11,2);
	cv_cpne NUMERIC(11,2) ;
	cv_cstd CHARACTER VARYING;
	cv_prne NUMERIC(11,2);
	cv_jobs NUMERIC ;
	cv_prpc NUMERIC(11,2) ;
	cv_prma NUMERIC(11,2) ;
	cv_dtim NUMERIC ;
	cv_dely NUMERIC ;
	cv_cdly NUMERIC ;
	cv_nale CHARACTER VARYING;
	cv_nasu CHARACTER VARYING;
	cv_nama CHARACTER VARYING;
	bkdt DATE;
	bktm CHARACTER VARYING;
	podt DATE;
	potm CHARACTER VARYING;
	dday DATE;
	dtim  CHARACTER VARYING;	
		
BEGIN		
	IF COALESCE(NEW.bkok,'N') = 'Y' THEN					
		-- CLIENT CREDIT UPDATE
		SELECT clmt, stop, clcd INTO client FROM CL WHERE CLID = NEW.clid;
			
		IF COALESCE(client.clmt, 0) != 0 AND COALESCE(client.stop, 'N') = 'Y' THEN					
			UPDATE cl SET ubam = get_credit_used(NEW.clid) WHERE CLID = NEW.clid; 
		END IF;	
		
		SELECT * INTO colAd from AD WHERE adid = NEW.cadr;		
		SELECT * INTO delAd from AD WHERE adid = NEW.dadr;
		cv_tpcl = 'N';
		IF COALESCE(colAd.type, 'XXX') IN ('CSC','CSD','CT') AND COALESCE(delAd.type, 'XXX') IN ('CSC','CSD','CT') THEN
			cv_tpcl := 'Y';
		END IF;
		
		-- cv_coag
		FOR leg IN SELECT jl.*, su.sucd FROM jl JOIN su ON su.suid = jl.suid WHERE jl.csid = NEW.csid AND jl.stat='L'
		LOOP			
			--RAISE NOTICE 'Job: % Leg: % Type: %', NEW.HAWB, leg.lgcd, leg.jlid;
			IF leg.type = 'C' AND cv_coag IS NULL  THEN  cv_coag = leg.sucd; END IF;
			IF leg.type = 'D' AND cv_deag IS NULL  THEN  
				cv_deag = leg.sucd; 
				cv_fdln = leg.lgcd;
				SELECT * INTO tleg FROM lg WHERE lg.lgid = leg.lgid;
				cv_fddt = tleg.etdp;
				cv_fdat = tleg.etav;
				IF leg.odel IS NOT NULL THEN
					IF UPPER(leg.odel) = 'COURIER' THEN cv_odel = 'Courier address'; END IF;					
					IF UPPER(leg.odel) = 'COLLECT' THEN cv_odel = 'Job''s collection address'; END IF;
					IF UPPER(leg.odel) NOT IN ('COURIER', 'COLLECT') THEN 
						SELECT lsvl INTO  cv_odel FROM LS WHERE lsid = leg.odel;
					END IF;					
				END IF;
				IF leg.ocol IS NOT NULL THEN
					IF UPPER(leg.ocol) = 'HOLD' THEN cv_ocol = 'Collect and hold'; END IF;					
					IF UPPER(leg.ocol) = 'DELIVERY' THEN cv_ocol = 'Job''s delivery address'; END IF;
					IF UPPER(leg.ocol) NOT IN ('HOLD', 'DELIVERY') THEN 
						SELECT lsvl INTO  cv_ocol FROM LS WHERE lsid = leg.ocol;
					END IF;					
				END IF;
				
			END IF;
			IF leg.type = 'D' AND cv_ftls IS NULL  THEN  
				cv_ftls = leg.sucd; 
				cv_ftln = leg.lgcd;
				SELECT * INTO tleg FROM lg WHERE lg.lgid = leg.lgid;
				cv_ftdt = tleg.etdp;
				cv_ftat = tleg.etav;
				SELECT * INTO manifest FROM mf WHERE mf.mfid = leg.mfid;
				cv_ftma = manifest.mfcd;
			END IF;
			
		END LOOP; 	
		
		-- POPULATE sales price SUMMERY
		SELECT SUM(pric) as cv_spne, SUM(CASE WHEN COALESCE(plac,'1') = '1' THEN pric END) AS sp01, SUM(CASE WHEN plac = '2' THEN pric END) AS sp02, SUM(CASE WHEN plac = '3' THEN pric END) AS sp03, SUM(CASE WHEN plac = '4' THEN pric END) AS sp04, SUM(CASE WHEN plac = '5' THEN pric END) AS sp05, SUM(CASE WHEN plac = '6' THEN pric END) AS sp06, SUM(CASE WHEN plac = '7' THEN pric END) AS sp07, SUM(CASE WHEN plac = '8' THEN pric END) AS sp08, SUM(CASE WHEN plac = '8' THEN pric END) AS sp09, SUM(CASE WHEN plac = '10' THEN pric END) AS sp10, SUM(CASE WHEN plac = '11' THEN pric END) AS sp11, SUM(CASE WHEN plac = '12' THEN pric END) AS sp12, SUM(CASE WHEN plac = '13' THEN pric END) AS sp13, SUM(CASE WHEN plac = '14' THEN pric END) AS sp14, SUM(CASE WHEN plac = '15' THEN pric END) AS sp15, SUM(CASE WHEN plac = '16' THEN pric END) AS sp16, SUM(CASE WHEN plac = '17' THEN pric END) AS sp17, SUM(CASE WHEN plac = '18' THEN pric END) AS sp18, SUM(CASE WHEN plac = '19' THEN pric END) AS sp19, SUM(CASE WHEN plac = '20' THEN pric END) AS sp20 INTO sales FROM JP WHERE jp.csid = NEW.csid AND stat='L';
		
		-- POPULATE cost price SUMMERY
		SELECT SUM(pric) as cv_cpne, SUM(CASE WHEN COALESCE(iref,'') = '' THEN 1 END) as costed, SUM(CASE WHEN COALESCE(plac,'1') = '1' THEN pric END) AS cp01, SUM(CASE WHEN plac = '2' THEN pric END) AS cp02, SUM(CASE WHEN plac = '3' THEN pric END) AS cp03, SUM(CASE WHEN plac = '4' THEN pric END) AS cp04, SUM(CASE WHEN plac = '5' THEN pric END) AS cp05, SUM(CASE WHEN plac = '6' THEN pric END) AS cp06, SUM(CASE WHEN plac = '7' THEN pric END) AS cp07, SUM(CASE WHEN plac = '8' THEN pric END) AS cp08, SUM(CASE WHEN plac = '8' THEN pric END) AS cp09, SUM(CASE WHEN plac = '10' THEN pric END) AS cp10, SUM(CASE WHEN plac = '11' THEN pric END) AS cp11, SUM(CASE WHEN plac = '12' THEN pric END) AS cp12, SUM(CASE WHEN plac = '13' THEN pric END) AS cp13, SUM(CASE WHEN plac = '14' THEN pric END) AS cp14, SUM(CASE WHEN plac = '15' THEN pric END) AS cp15, SUM(CASE WHEN plac = '16' THEN pric END) AS cp16, SUM(CASE WHEN plac = '17' THEN pric END) AS cp17, SUM(CASE WHEN plac = '18' THEN pric END) AS cp18, SUM(CASE WHEN plac = '19' THEN pric END) AS cp19, SUM(CASE WHEN plac = '20' THEN pric END) AS cp20  INTO costs FROM ci  WHERE ci.csid = NEW.csid AND ci.stat='L';
		cv_cstd = 'Y';
		IF costs.costed > 0 THEN cv_cstd = 'N'; END IF;
		cv_prne = COALESCE(sales.cv_spne, 0) - COALESCE(costs.cv_cpne,0);
		cv_jobs = 1;
		-- Profit percentage (profit / sale price)
		cv_prpc = COALESCE(sales.cv_spne, 1);
		IF cv_prpc = 0 THEN cv_prpc = 1; END IF;		
		cv_prpc = (COALESCE(cv_prne,0) / cv_prpc) * 100;
		-- Profit margin (profit / cost price)
		cv_prma = COALESCE(costs.cv_cpne, 1);
		IF cv_prma = 0 THEN cv_prma = 1; END IF;		
		cv_prma = (COALESCE(cv_prne,0) / cv_prma) * 100;
		-- Delivery time
		dtim = NEW.dday || ' ' || COALESCE(NEW.dtim,'00:00');
		IF NEW.skey = 'BYANRN' OR NEW.skey = 'CNRNAY' THEN
			cv_cdly = getCollectionDelay(NEW.bkdt, NEW.bktm, NEW.rday, NEW.rtim,  NEW.skey);
		END IF;
		
		cv_dtim = EXTRACT(EPOCH FROM (dtim::timestamp - bkdt::timestamp))/60;	
		cv_dely = getDeliveryDelay(NEW.bkdt, NEW.bktm, NEW.rday, NEW.rtim, NEW.dday, NEW.dtim, NEW.podt, NEW.potm, NEW.skey, NEW.dlok);
		
		RAISE NOTICE 'Collection delay: % ', cv_cdly;
		
		-- UPDATE CV salesORD
		SELECT cv.csid INTO consignmentId FROM CV WHERE CV.csid = NEW.csid;
			
		IF consignmentId IS NULL THEN		-- add
			INSERT INTO CV (csid) VALUES (NEW.csid);
		END IF;
		UPDATE CV SET 
			frco = colAd.cocd, frpl = colAd.plcd, toco = colAd.cocd, topl = colAd.plcd, tpcl = cv_tpcl, odel = cv_odel, ocol = cv_ocol,
			coag = cv_coag, deag = cv_deag, fdln = cv_fdln, fddt = cv_fddt, fdat = cv_fdat, 
			ftls = cv_ftls, ftln = cv_ftln, ftdt = cv_ftdt, ftat = cv_ftat, ftma = cv_ftma, dtim = cv_dtim, dely = cv_dely, cdly = cv_cdly,
			spne = sales.cv_spne, cpne = costs.cv_cpne, jobs = cv_jobs, prne = cv_prne, prpc = cv_prpc, prma = cv_prma,
			sp01 = sales.sp01, sp02 = sales.sp02, sp03 = sales.sp03, sp04 = sales.sp04, sp05 = sales.sp05, sp06 = sales.sp06, sp07 = sales.sp07, sp08 = sales.sp08, sp09 = sales.sp09, sp10 = sales.sp10, sp11 = sales.sp11, sp12 = sales.sp12, sp13 = sales.sp13, sp14 = sales.sp14, sp15 = sales.sp15, sp16 = sales.sp16, sp17 = sales.sp17, sp18 = sales.sp18, sp19 = sales.sp19, sp20 = sales.sp20, 
			cp01 = costs.cp01, cp02 = costs.cp02, cp03 = costs.cp03, cp04 = costs.cp04, cp05 = costs.cp05, cp06 = costs.cp06, cp07 = costs.cp07, cp08 = costs.cp08, cp09 = costs.cp09, cp10 = costs.cp10, cp11 = costs.cp11, cp12 = costs.cp12, cp13 = costs.cp13, cp14 = costs.cp14, cp15 = costs.cp15, cp16 = costs.cp16, cp17 = costs.cp17, cp18 = costs.cp18, cp19 = costs.cp19, cp20 = costs.cp20 
		WHERE CV.csid = NEW.csid;
		
		-- ALSO UPDATE WE STATUS IF CS	
		BEGIN
			IF COALESCE(OLD.skey,'') != COALESCE(NEW.SKEY,'') THEN		
				RAISE NOTICE 'Update piece status %  %', OLD.skey, NEW.skey;
				EXECUTE updatePieceStatus(NEW.csid);
			ELSIF COALESCE(OLD.dvok,'') != COALESCE(NEW.dvok,'') THEN
				RAISE NOTICE 'Update piece status %  %', OLD.skey, NEW.skey;
				EXECUTE updatePieceStatus(NEW.csid);
			END IF;	
		EXCEPTION
		    WHEN others THEN RAISE NOTICE 'Ignore';
		END;
	
	END IF;
		
	RETURN NEW;
END;
$$;
ALTER FUNCTION public.cs_update_summery() OWNER TO ncuser;
CREATE FUNCTION date2text(date) RETURNS character varying
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$select $1::text$_$;
ALTER FUNCTION public.date2text(date) OWNER TO ncuser;
CREATE FUNCTION delete_search_data(rec_type character varying, rec_id character varying) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
	--RAISE NOTICE 'Deleting type: % record:%', rec_type, rec_id;
	DELETE FROM search WHERE recordtype =  rec_type AND  recordid = rec_id;
END;
$$;
ALTER FUNCTION public.delete_search_data(rec_type character varying, rec_id character varying) OWNER TO ncuser;
CREATE FUNCTION encrypt_password_data_patch() RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
	pwLS RECORD;
	contacts CURSOR FOR SELECT * FROM ct WHERE usnm IS NOT NULL AND pass IS NOT NULL AND salt IS NULL; 
	contact RECORD;
	users CURSOR FOR SELECT * FROM us WHERE pass IS NOT NULL AND salt IS NULL; 
	usr RECORD;
	pswd character varying;
	psalt character varying;
	rc RECORD;
BEGIN
	
	-- check if password encryption is enabled or not
	select * into pwLS from LS WHERE LSPX='CR' AND LSVL='PSWDENCT' AND STAT='L';
	IF pwLS IS NULL THEN
		RAISE NOTICE 'Password encryption not enabled';		
		RETURN;
	END IF;
	
	-- check if password encryption is already done or not
	IF pwLS.seqn = 'Y' OR pwLS.seqn = 'y' THEN
		RAISE NOTICE 'Password encryption enabled';		
	ELSE
		RAISE NOTICE 'Password encryption not enabled';		
		RETURN;
	END IF;
	IF pwLS.exv2 = 'Y' OR pwLS.exv2 = 'y' THEN
		RAISE NOTICE 'Data patch for password encryption already run. Thus Skipping the data patch';		
		RETURN;
	END IF;
	RAISE NOTICE '========== STARTING DATA PATCH FOR PASSWORD ==========';
	
	
	-- encrypt contact's pass
	
	FOR contact IN contacts LOOP
		psalt = gen_salt('bf',8);
		pswd = crypt(contact.pass, psalt);
		UPDATE ct SET pass= pswd, salt = psalt, opls = null WHERE "CTID" = contact."CTID";
		SELECT * into rc FROM ct WHERE "CTID" = contact."CTID";
		RAISE NOTICE 'Updated password hash for %', contact."CTID";
		
	END LOOP;
	-- encrypt user's password
	FOR usr IN users LOOP
		psalt = gen_salt('bf',8);
		pswd = crypt(usr.pass, psalt);
		UPDATE us SET pass= pswd, salt = psalt, opls = null WHERE usid = usr.usid;
		SELECT * into rc FROM us WHERE usid = usr.usid;
		RAISE NOTICE '*Updated password hash for %', usr.usid;
		
	END LOOP;	
END;
$$;
ALTER FUNCTION public.encrypt_password_data_patch() OWNER TO ncuser;
CREATE FUNCTION get_co() RETURNS integer
    LANGUAGE plpgsql
    AS $$
DECLARE
    r RECORD;
BEGIN
    SELECT into r * FROM CO WHERE norm_key_val(ARRAY['C08','I17'],ARRAY[COID::text,(_BOOKMARK_&36028797018963967)::text])>='3030303030323330' AND norm_key_val(ARRAY['C08','I17'],ARRAY[COID::text,(_BOOKMARK_&36028797018963967)::text])<='FF' LIMIT 1;
    RETURN 1;
   
END;
$$;
ALTER FUNCTION public.get_co() OWNER TO ncuser;
CREATE FUNCTION get_credit_used(clientid character varying) RETURNS numeric
    LANGUAGE plpgsql
    AS $$
DECLARE
	amount numeric;
	c_amount numeric;
	r_amount numeric;
BEGIN
	SELECT sum(qprc) into c_amount FROM cs WHERE ikey='C' AND clid= clientId AND stat='L' AND bkok='Y';
	SELECT sum(qprc) into r_amount FROM cs WHERE ikey='R' AND clid= clientId AND stat='L' AND bkok='Y';
	IF c_amount IS NULL THEN c_amount = 0; END IF;
	IF r_amount IS NULL THEN r_amount = 0; END IF;
	
	amount = c_amount + r_amount;	
	
	RETURN amount;	
END
$$;
ALTER FUNCTION public.get_credit_used(clientid character varying) OWNER TO ncuser;
CREATE FUNCTION get_max_number(size integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
	num character varying;
BEGIN
	num := '';
	FOR i IN 1..size LOOP
		num := num || '9';
	END LOOP;
	
	RETURN num::bigint;	
END
$$;
ALTER FUNCTION public.get_max_number(size integer) OWNER TO ncuser;
CREATE FUNCTION get_min_number(size integer) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
	num character varying;	
BEGIN
	num := '1';
	FOR i IN 1..(size-1) LOOP
		num := num || '0';
	END LOOP;
	
	RETURN num::bigint;	
END
$$;
ALTER FUNCTION public.get_min_number(size integer) OWNER TO ncuser;
CREATE FUNCTION getcollectiondelay(bkdt date, bktm character varying, rday date, rtim character varying, skey character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
	num BIGINT;
BEGIN
	IF skey = 'BYANRN' OR skey = 'CNRNAY' THEN
		bkdt = COALESCE(rday,bkdt);
		IF bktm = '' THEN bktm = null ; END IF;
		IF rtim = '' THEN rtim = null ; END IF;
		bktm = bkdt || ' ' || COALESCE(COALESCE(rtim,bktm));		
		Select (EXTRACT(EPOCH FROM (now()::timestamp - bktm::timestamp))/60)::BIGINT into num;	
	END IF;
	RETURN num;
END
$$;
ALTER FUNCTION public.getcollectiondelay(bkdt date, bktm character varying, rday date, rtim character varying, skey character varying) OWNER TO ncuser;
CREATE FUNCTION getdeliverydelay(bkdt date, bktm character varying, rday date, rtim character varying, podt date, potm character varying, skey character varying, dlok character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
	num BIGINT;
BEGIN
	IF skey = 'BYANRN' OR skey = 'CNRNAY' THEN
		bkdt = COALESCE(rday,bkdt);
		IF bktm = '' THEN bktm = null ; END IF;
		IF rtim = '' THEN rtim = null ; END IF;
		bktm = bkdt || ' ' || COALESCE(COALESCE(rtim,bktm));		
		Select (EXTRACT(EPOCH FROM (now()::timestamp - bktm::timestamp))/60)::BIGINT into num;	
	END IF;
	RETURN num;
END
$$;
ALTER FUNCTION public.getdeliverydelay(bkdt date, bktm character varying, rday date, rtim character varying, podt date, potm character varying, skey character varying, dlok character varying) OWNER TO ncuser;
CREATE FUNCTION getdeliverydelay(bkdt date, bktm character varying, rday date, rtim character varying, dday date, dtim character varying, podt date, potm character varying, skey character varying, dlok character varying) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
	cv_dely BIGINT;
	cv_dtim BIGINT;
BEGIN
	-- DELAY CALCULATION		
	bkdt = COALESCE(rday,bkdt);
	IF bktm = '' THEN bktm = null ; END IF;
	IF rtim = '' THEN rtim = null ; END IF;
	bktm = bkdt || ' ' || COALESCE(COALESCE(rtim,bktm));	
	dtim = dday || ' ' || COALESCE(dtim,'00:00');
	potm = to_char(now(),'YYYY-mm-dd HH:MM'); -- Current date as delvery date
			
	IF dlok = 'Y' THEN
		podt = COALESCE(podt,now());
		IF potm = '' THEN  potm = null; END IF;
		potm = podt || ' ' || COALESCE(potm,'00:00');
	END IF;
	cv_dtim = EXTRACT(EPOCH FROM (dtim::timestamp - bkdt::timestamp))/60;	
	cv_dely = EXTRACT(EPOCH FROM (potm::timestamp - bkdt::timestamp))/60 - cv_dtim;
		
	RETURN cv_dely;
END
$$;
ALTER FUNCTION public.getdeliverydelay(bkdt date, bktm character varying, rday date, rtim character varying, dday date, dtim character varying, podt date, potm character varying, skey character varying, dlok character varying) OWNER TO ncuser;
CREATE FUNCTION getnextawbnumber(productid character varying, reserved bigint) RETURNS character varying
    LANGUAGE plpgsql
    AS $$
DECLARE
	next_id int8;
BEGIN
	IF reserved = 0 THEN reserved := 1; END IF;
	
	UPDATE pd SET nawb = nawb + reserved WHERE pdid = productId;
	SELECT INTO next_id nawb from pd WHERE pdid = productId;
	RETURN next_id::character varying;
END
$$;
ALTER FUNCTION public.getnextawbnumber(productid character varying, reserved bigint) OWNER TO ncuser;
CREATE FUNCTION gettimesince(mydate date) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
	num BIGINT;
BEGIN
	Select (EXTRACT(EPOCH FROM (now()::timestamp - mydate::timestamp))/60)::BIGINT into num;
	
	RETURN num;	
END
$$;
ALTER FUNCTION public.gettimesince(mydate date) OWNER TO ncuser;
CREATE FUNCTION gettimesince(date1 timestamp without time zone, date2 timestamp without time zone) RETURNS bigint
    LANGUAGE plpgsql
    AS $$
DECLARE
	num BIGINT;
BEGIN
	Select (EXTRACT(EPOCH FROM (date1::timestamp - date2::timestamp))/60)::BIGINT into num;	
	RETURN num;	
END
$$;
ALTER FUNCTION public.gettimesince(date1 timestamp without time zone, date2 timestamp without time zone) OWNER TO ncuser;
CREATE FUNCTION id_generator(OUT result bigint) RETURNS bigint
    LANGUAGE plpgsql
    AS $$  
DECLARE  
    our_epoch bigint := 1314220021721;
    seq_id bigint;
    now_millis bigint;
    shard_id int := 1;
BEGIN  
    SELECT nextval('global_id_sequence') % 1024 INTO seq_id;
    RAISE NOTICE 'Mod: %', seq_id;
    SELECT FLOOR(EXTRACT(EPOCH FROM clock_timestamp()) * 1000) INTO now_millis;
    RAISE NOTICE 'Now (ms): %', now_millis;
    RAISE NOTICE '%', ((shard_id << 10));
    result := (now_millis - our_epoch) << 23;
    RAISE NOTICE 'result 1: %', result;    
    result := result | (shard_id << 10);    
    RAISE NOTICE 'result 2: %', result;
    result := result | (seq_id);    
    RAISE NOTICE 'result 3: %', result;
END;  
$$;
ALTER FUNCTION public.id_generator(OUT result bigint) OWNER TO ncuser;
CREATE FUNCTION increment(i integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
BEGIN
	RETURN i + 1;
END;
$$;
ALTER FUNCTION public.increment(i integer) OWNER TO ncuser;
CREATE FUNCTION jl_update_job() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
	PERFORM update_job_summery(NEW.csid);
	RETURN NEW;
END;
$$;
ALTER FUNCTION public.jl_update_job() OWNER TO ncuser;
CREATE FUNCTION norm_key_val(text[], text[]) RETURNS text
    LANGUAGE c IMMUTABLE
    AS '/usr/lib/mfpgex.so', 'norm_key_val';
ALTER FUNCTION public.norm_key_val(text[], text[]) OWNER TO ncuser;
CREATE FUNCTION nt_update_summery() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE 
	r record;
	changes hstore;
BEGIN
	changes := hstore(NEW) - hstore(OLD);
	FOR r IN SELECT (each(changes)).* LOOP
		Raise notice 'Changes % = %', r.key, r.value;
	END LOOP;
  
	RETURN NEW;
END;
$$;
ALTER FUNCTION public.nt_update_summery() OWNER TO ncuser;
CREATE FUNCTION on_record_update() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
	phrase TSVECTOR;
	client RECORD;
	fromPlace RECORD;
	toPlace RECORD;
	service RECORD;
BEGIN
	IF TG_OP = 'INSERT' OR TG_OP = 'UPDATE' THEN
		--RAISE NOTICE 'Update: %', TG_TABLE_NAME;	
		IF TG_TABLE_NAME = 'cs' AND NEW.STAT != 'L' THEN
			IF coalesce(NEW.HAWB,NEW.circ) IS NOT NULL THEN						-- FOR CS/Job
				SELECT * INTO client FROM cl WHERE clid = NEW.clid;
				SELECT * INTO fromPlace FROM pl WHERE plid = NEW.fpid;
				SELECT * INTO toPlace FROM pl WHERE plid = NEW.tpid;
				SELECT * INTO service FROM ls WHERE lsid = NEW.slid;
				
				phrase := 	setweight(to_tsvector(coalesce(NEW.hawb,'')), 'A') ||
						setweight(to_tsvector(coalesce(NEW.tref,'')), 'B') ||
						setweight(to_tsvector(coalesce(client."DESC",'')), 'B') ||
						setweight(to_tsvector(coalesce(client.clcd,'')), 'B') ||
						setweight(to_tsvector(coalesce(fromPlace."DESC",'')), 'C') ||
						setweight(to_tsvector(coalesce(fromPlace.altn,'')), 'C') ||
						setweight(to_tsvector(coalesce(fromPlace.cocd,'')), 'C') ||
						setweight(to_tsvector(coalesce(fromPlace.countryname,'')), 'C') ||
						setweight(to_tsvector(coalesce(toPlace."DESC",'')), 'C') ||
						setweight(to_tsvector(coalesce(toPlace.altn,'')), 'C') ||
						setweight(to_tsvector(coalesce(toPlace.cocd,'')), 'C') ||
						setweight(to_tsvector(coalesce(toPlace.countryname,'')), 'C') ||
						setweight(to_tsvector(coalesce(service.lsvl,'')), 'B') ||
						setweight(to_tsvector(coalesce(service.note,'')), 'B') ||
						setweight(to_tsvector(coalesce(NEW.inbn,'')), 'C') ||
						setweight(to_tsvector(coalesce(NEW.cref,'')), 'D') ||
						setweight(to_tsvector(coalesce(NEW.crf2,'')), 'D') ||
						setweight(to_tsvector(coalesce(NEW.crf3,'')), 'D') ;
				PERFORM	update_search_data('CS', NEW.csid, coalesce(NEW.HAWB,NEW.circ),phrase);
			END IF;
		ELSE
			SELECT delete_search_data(TG_TABLE_NAME, OLD.csid);
		END IF;
		RETURN NEW;
	END IF;
	-- For DELETE
	IF TG_OP = 'DELETE' THEN
		RAISE NOTICE 'Delete: %', TG_TABLE_NAME;	
		IF TG_TABLE_NAME = 'cs' THEN				-- FOR CS/Job
			SELECT delete_search_data(TG_TABLE_NAME, OLD.csid);
		END IF;
		RETURN OLD;
	END IF;
	RETURN NEW;
	
END;
$$;
ALTER FUNCTION public.on_record_update() OWNER TO ncuser;
CREATE FUNCTION on_record_update_cl() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
	phrase TSVECTOR;	
	mainaddress RECORD;	
	invoiceaddress RECORD;
	defaultaddress RECORD;
BEGIN
	IF TG_OP = 'INSERT' OR TG_OP = 'UPDATE' THEN
		--RAISE NOTICE 'Update: %', TG_TABLE_NAME;	
		IF TG_TABLE_NAME = 'cl' AND NEW.STAT != 'D' THEN
			IF coalesce(NEW.clcd,NEW."DESC") IS NOT NULL THEN
				SELECT * INTO mainaddress FROM ad WHERE adid = NEW.cadr;
				SELECT * INTO invoiceaddress FROM ad WHERE adid = NEW.iadr;
				SELECT * INTO defaultaddress FROM ad WHERE adid = NEW.dadr;
				
					phrase := 	setweight(to_tsvector(coalesce(NEW.clcd,'')), 'A') ||
							setweight(to_tsvector(coalesce(NEW."DESC",'')), 'A') ||
							setweight(to_tsvector(coalesce(NEW.ieml,'')), 'C') ||
							setweight(to_tsvector(coalesce(mainaddress.adr1,'')), 'B') ||
							setweight(to_tsvector(coalesce(mainaddress.adr2,'')), 'C') ||
							setweight(to_tsvector(coalesce(mainaddress.adr3,'')), 'C') ||
							setweight(to_tsvector(coalesce(mainaddress.adr4,'')), 'B') ||
							setweight(to_tsvector(coalesce(mainaddress.zpcd,'')), 'B') ||
							setweight(to_tsvector(coalesce(invoiceaddress.adr1,'')), 'B') ||
							setweight(to_tsvector(coalesce(invoiceaddress.adr2,'')), 'C') ||
							setweight(to_tsvector(coalesce(invoiceaddress.adr3,'')), 'C') ||
							setweight(to_tsvector(coalesce(invoiceaddress.adr4,'')), 'B') ||
							setweight(to_tsvector(coalesce(invoiceaddress.zpcd,'')), 'B') ||
							setweight(to_tsvector(coalesce(defaultaddress.adr1,'')), 'B') ||
							setweight(to_tsvector(coalesce(defaultaddress.adr2,'')), 'C') ||
							setweight(to_tsvector(coalesce(defaultaddress.adr3,'')), 'C') ||
							setweight(to_tsvector(coalesce(defaultaddress.adr4,'')), 'B') ||
							setweight(to_tsvector(coalesce(defaultaddress.zpcd,'')), 'B');
							
					PERFORM	update_search_data('CL', NEW.clid, coalesce(NEW.clid,NEW."DESC"),phrase);
			END IF;
		ELSE			
			PERFORM delete_search_data(TG_TABLE_NAME::CHARACTER VARYING, OLD.clid);
		END IF;
		RETURN NEW;
	END IF;
	-- For DELETE
	IF TG_OP = 'DELETE' THEN
		RAISE NOTICE 'Delete: %', TG_TABLE_NAME;	
		PERFORM delete_search_data(TG_TABLE_NAME::CHARACTER VARYING, OLD.clid);
		RETURN OLD;
	END IF;
	RETURN NEW;
	
END;
$$;
ALTER FUNCTION public.on_record_update_cl() OWNER TO ncuser;
CREATE FUNCTION on_record_update_cs() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
	phrase TSVECTOR;
	client RECORD;
	fromPlace RECORD;
	toPlace RECORD;
	service RECORD;
BEGIN
	IF TG_OP = 'INSERT' OR TG_OP = 'UPDATE' THEN
		--RAISE NOTICE 'Update: %', TG_TABLE_NAME;	
		IF TG_TABLE_NAME = 'cs' AND NEW.STAT != 'D' THEN
			IF coalesce(NEW.HAWB,NEW.circ) IS NOT NULL THEN						-- FOR CS/Job
				SELECT * INTO client FROM cl WHERE clid = NEW.clid;
				SELECT * INTO fromPlace FROM pl WHERE plid = NEW.fpid;
				SELECT * INTO toPlace FROM pl WHERE plid = NEW.tpid;
				SELECT * INTO service FROM ls WHERE lsid = NEW.slid;
				
				phrase := 	setweight(to_tsvector(coalesce(NEW.hawb,'')), 'A') ||
						setweight(to_tsvector(coalesce(NEW.tref,'')), 'B') ||
						setweight(to_tsvector(coalesce(client."DESC",'')), 'B') ||
						setweight(to_tsvector(coalesce(client.clcd,'')), 'B') ||
						setweight(to_tsvector(coalesce(fromPlace."DESC",'')), 'C') ||
						setweight(to_tsvector(coalesce(fromPlace.altn,'')), 'C') ||
						setweight(to_tsvector(coalesce(fromPlace.cocd,'')), 'C') ||
						setweight(to_tsvector(coalesce(fromPlace.countryname,'')), 'C') ||
						setweight(to_tsvector(coalesce(toPlace."DESC",'')), 'C') ||
						setweight(to_tsvector(coalesce(toPlace.altn,'')), 'C') ||
						setweight(to_tsvector(coalesce(toPlace.cocd,'')), 'C') ||
						setweight(to_tsvector(coalesce(toPlace.countryname,'')), 'C') ||
						setweight(to_tsvector(coalesce(service.lsvl,'')), 'B') ||
						setweight(to_tsvector(coalesce(service.note,'')), 'B') ||
						setweight(to_tsvector(coalesce(NEW.inbn,'')), 'C') ||
						setweight(to_tsvector(coalesce(NEW.cref,'')), 'D') ||
						setweight(to_tsvector(coalesce(NEW.crf2,'')), 'D') ||
						setweight(to_tsvector(coalesce(NEW.crf3,'')), 'D') ;
				PERFORM	update_search_data('CS', NEW.csid, coalesce(NEW.HAWB,NEW.circ),phrase);
			END IF;
		ELSE
			PERFORM delete_search_data(TG_TABLE_NAME::CHARACTER VARYING, OLD.csid);
		END IF;
		RETURN NEW;
	END IF;
	-- For DELETE
	IF TG_OP = 'DELETE' THEN
		RAISE NOTICE 'Delete: %', TG_TABLE_NAME;	
		PERFORM delete_search_data(TG_TABLE_NAME::CHARACTER VARYING, OLD.csid);
		RETURN OLD;
	END IF;
	RETURN NEW;
	
END;
$$;
ALTER FUNCTION public.on_record_update_cs() OWNER TO ncuser;
CREATE FUNCTION on_record_update_dc() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
	phrase TSVECTOR;
	consignment RECORD;
	users RECORD;
BEGIN
	IF TG_OP = 'INSERT' OR TG_OP = 'UPDATE' THEN
		--RAISE NOTICE 'Update: %', TG_TABLE_NAME;	
		IF TG_TABLE_NAME = 'dc' AND NEW.STAT != 'D' THEN
			IF coalesce(NEW.dcid,NEW.dccd) IS NOT NULL THEN				
				SELECT * INTO consignment FROM cs WHERE csid = NEW.dccd;
				SELECT * INTO users FROM us WHERE usid = NEW.crus;
				
					phrase := 	setweight(to_tsvector(coalesce(NEW."DESC",'')), 'A') ||
								setweight(to_tsvector(coalesce(users.uscd,'')), 'C') ||															
								setweight(to_tsvector(coalesce(consignment.hawb,'')), 'B');
							
					PERFORM	update_search_data('DC', NEW.dcid, coalesce(NEW.dccd,NEW."DESC"),phrase);
			END IF;
		ELSE			
			PERFORM delete_search_data(TG_TABLE_NAME::CHARACTER VARYING, OLD.dcid);
		END IF;
		RETURN NEW;
	END IF;
	-- For DELETE
	IF TG_OP = 'DELETE' THEN
		RAISE NOTICE 'Delete: %', TG_TABLE_NAME;
		PERFORM delete_search_data(TG_TABLE_NAME::CHARACTER VARYING, OLD.dcid);
		RETURN OLD;
	END IF;
	RETURN NEW;
	
END;
$$;
ALTER FUNCTION public.on_record_update_dc() OWNER TO ncuser;
CREATE FUNCTION on_record_update_nt() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
	phrase TSVECTOR;
	users RECORD;
	supervisor RECORD;
BEGIN
	IF TG_OP = 'INSERT' OR TG_OP = 'UPDATE' THEN
		--RAISE NOTICE 'Update: %', TG_TABLE_NAME;	
		IF TG_TABLE_NAME = 'nt' AND NEW.STAT != 'D' THEN
			IF coalesce(NEW.task,NEW.note) IS NOT NULL THEN
				SELECT * INTO users FROM us WHERE usid = NEW.asto;
				SELECT * INTO supervisor FROM us WHERE usid = NEW.svid;
				
					phrase := 	setweight(to_tsvector(coalesce(NEW.task,'')), 'A') ||
								setweight(to_tsvector(coalesce(NEW.pobn,'')), 'B') ||
								setweight(to_tsvector(coalesce(users.uscd,'')), 'B') ||
								setweight(to_tsvector(coalesce(supervisor.uscd,'')), 'C');
							
					PERFORM	update_search_data('NT', NEW.ntid, coalesce(NEW.task,NEW.note),phrase);
			END IF;
		ELSE			
			PERFORM delete_search_data(TG_TABLE_NAME::CHARACTER VARYING, OLD.ntid);
		END IF;
		RETURN NEW;
	END IF;
	-- For DELETE
	IF TG_OP = 'DELETE' THEN
		RAISE NOTICE 'Delete: %', TG_TABLE_NAME;	
		PERFORM delete_search_data(TG_TABLE_NAME::CHARACTER VARYING, OLD.ntid);
		RETURN OLD;
	END IF;
	RETURN NEW;
	
END;
$$;
ALTER FUNCTION public.on_record_update_nt() OWNER TO ncuser;
CREATE FUNCTION onupdatenumberseries() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
	seq_name character varying;
BEGIN
	IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE') THEN
		seq_name := 'number_series_' || LOWER(NEW.refr) || '_seq';		
		PERFORM create_custom_series(seq_name, NEW.nnum, NEW.dnum::INT);
		RETURN NULL; 
	END IF;
END;
$$;
ALTER FUNCTION public.onupdatenumberseries() OWNER TO ncuser;
CREATE FUNCTION pl_update_search_pharase() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  country_code varchar;
  country_name varchar;
BEGIN
	SELECT cocd, "DESC" INTO country_code, country_name  FROM co WHERE co.coid = NEW.coid;
	
	NEW.cocd :=  country_code;
	NEW.countryName :=  country_name;
	IF NEW.type = 'PC' THEN
		NEW.psfr_lpad := LPAD(NEW.psfr, 30, '0');	
		NEW.altn_lpad := LPAD(NEW.altn, 30, '0');
	END IF;
	NEW.search_phrase :=
	    setweight(to_tsvector(coalesce(new."DESC",'')), 'A') ||
		setweight(to_tsvector(coalesce(new.altn,'')), 'A') ||
		setweight(to_tsvector(coalesce(new.psfr,'')), 'B') ||
		setweight(to_tsvector(coalesce(new.psto,'')), 'B') ||
		setweight(to_tsvector(coalesce(new.adr1,'')), 'A') ||
	    setweight(to_tsvector(coalesce(new.adr2,'')), 'A') ||
	    setweight(to_tsvector(coalesce(new.adr3,'')), 'A') ||
	    setweight(to_tsvector(coalesce(new.adr4,'')), 'A') ||
	    setweight(to_tsvector(coalesce(country_code,'')), 'C') ||
	    setweight(to_tsvector(coalesce(country_name,'')), 'C');	  
	RETURN NEW;
END;
$$;
ALTER FUNCTION public.pl_update_search_pharase() OWNER TO ncuser;
CREATE FUNCTION product_awb_series_check() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE	
BEGIN
	IF (TG_OP = 'UPDATE') THEN
		IF old.nawb IS NULL OR old.nawb = 0 THEN 
			RETURN NEW ;
		END IF;
		IF new.nawb IS NULL  OR new.nawb = 0 THEN 
			RAISE EXCEPTION 'Next AWB number is null or empty';
		END IF;
		IF new.nawb < old.nawb THEN 
			RAISE EXCEPTION 'Next AWB number smaller then previous value';
		END IF;
		RETURN NEW ;
	END IF;
END;
$$;
ALTER FUNCTION public.product_awb_series_check() OWNER TO ncuser;
CREATE FUNCTION search_address(searchtext character varying, ad_type character varying, refr character varying, contactid character varying, countryid character varying) RETURNS refcursor
    LANGUAGE plpgsql
    AS $$ 
DECLARE
	addresses refcursor;           -- Declare cursor variables     
	sql character varying;
	typeCont character varying;
	
	clientId character varying;
	masterId character varying;
	searchToken character varying;
BEGIN  
	searchToken = replace(trim(BOTH ' ' FROM regexp_replace(lower(searchtext),'\s+',' ','g')), ' ', ':* & ') || ':*';	
	
	
	sql = 	'SELECT ad.* , ts_rank_cd(search_phrase, q) AS rank ' ||
		'FROM ad, to_tsquery(' || quote_literal(searchToken) || ') AS q  ' ||
		'WHERE (search_phrase @@ q) ' ||
		' AND stat = ''L'' ' ||
		' AND COALESCE(supf,''N'') = ''N'' ' ||
		' AND cocd IS NOT NULL '  ||
		' AND plid IS NOT NULL '  ;
	ad_type = UPPER(ad_type);
	typeCont =  ' AND type || clid = ' || quote_literal(ad_type || refr)  ;
	
	IF ad_type IN ('CSC','CSD','BTH') THEN		
	
		SELECT clid, mbcl INTO clientId, masterId FROM cl WHERe clid = refr;		
		IF clientId is NULL THEN clientId = 'XXXXXXXX'; END IF;
		
		typeCont =  quote_literal('CSC'|| clientId) || ',' || quote_literal('CSD'|| clientId) || ',' || quote_literal('BTH'|| clientId) ;
		typeCont =  typeCont || ',' || quote_literal('CSCZZGLOBAL') || ',' || quote_literal('CSDZZGLOBAL') || ',' || quote_literal('BTHZZGLOBAL') ;
		
		IF masterId IS NOT NULL THEN
			typeCont =  typeCont || ',' || quote_literal('CSC'|| masterId) || ',' || quote_literal('CSD'|| masterId) || ',' || quote_literal('BTH'|| masterId) ;
		END IF;
		
		
		IF contactId IS NOT NULL THEN 
			typeCont =  typeCont || ',' || quote_literal('CT'|| contactId) ;
		END IF;
		
		typeCont = ' AND type || clid IN (' || typeCont || ') ';
	END IF;
	
	
	IF typeCont IS NOT NULL THEN
		sql = sql || typeCont;
	END IF;
	
	IF countryId IS NOT NULL THEN
		sql = sql || ' AND ad.coid = ''' || countryId || ''' ';
	END IF;
		
	sql  = sql || ' ORDER BY rank DESC, hits DESC NULLS LAST LIMIT 25';
	RAISE NOTICE '%', sql;
	
	OPEN addresses FOR EXECUTE sql;
	
	RETURN addresses;
END
$$;
ALTER FUNCTION public.search_address(searchtext character varying, ad_type character varying, refr character varying, contactid character varying, countryid character varying) OWNER TO ncuser;
CREATE FUNCTION search_place(searchtext character varying, co_id character varying) RETURNS refcursor
    LANGUAGE plpgsql
    AS $$ 
DECLARE
	places refcursor;           -- Declare cursor variables     
	sql character varying;
	countryId character varying;
	allowAoD character varying;
	allowPort character varying;
	allowCity character varying;
	allowPost character varying;
	allowOther character varying;
	condition character varying;
	typeCond  character varying;
	searchToken character varying;
BEGIN  
	searchToken = replace(trim(BOTH ' ' FROM regexp_replace(lower(searchtext),'\s+',' ','g')), ' ', ':* & ') || ':*';	
	
	SELECT coid, aaod, acty, apcd, aprt, aoth INTO countryId, allowAoD, allowCity, allowPost, allowPort, allowOther FROM co WHERE coid = co_id;
	
	condition = '';
	IF countryId IS NOT NULL THEN
		condition = ' AND pl.coid = '|| quote_literal(countryId) ;
	END IF;
	
	typeCond = 	' AND CASE WHEN pl.type = ''CT'' AND COALESCE (co.acty,''N'') = ''N'' THEN FALSE ELSE TRUE END' ||
				' AND CASE WHEN pl.type = ''SR'' AND COALESCE (co.acty,''N'') = ''N'' THEN FALSE ELSE TRUE END' ||
				' AND CASE WHEN pl.type = ''PO'' AND COALESCE (co.aprt,''N'') = ''N'' THEN FALSE ELSE TRUE END' ||
				' AND CASE WHEN pl.type = ''PC'' AND COALESCE (co.apcd,''N'') = ''N'' THEN FALSE ELSE TRUE END' ||
				' AND CASE WHEN pl.type = ''OD'' AND COALESCE (co.aaod,''N'') = ''N'' THEN FALSE ELSE TRUE END' ||
				' AND CASE WHEN pl.type = ''OT'' AND COALESCE (co.aoth,''N'') = ''N'' THEN FALSE ELSE TRUE END' ;
	
	sql = 	'SELECT pl.* , ts_rank_cd(search_phrase, q) AS rank ' ||
		' FROM pl JOIN co ON co.coid = pl.coid , to_tsquery('|| quote_literal(searchToken) ||') AS q  ' ||
		' WHERE (search_phrase @@ q) AND pl."DESC" != ' || quote_literal('AOD') ||
		' AND pl.stat = ''L'' ' ||
		' AND pl.cocd IS NOT NULL   ' ||  condition ||  typeCond  ||
		' ORDER BY rank DESC, weig DESC NULLS LAST LIMIT 25';
	RAISE NOTICE '%', sql;
	OPEN places FOR EXECUTE sql;
	RETURN places;
END
$$;
ALTER FUNCTION public.search_place(searchtext character varying, co_id character varying) OWNER TO ncuser;
CREATE FUNCTION search_records(searchtext character varying) RETURNS refcursor
    LANGUAGE plpgsql
    AS $$ 
DECLARE
	records REFCURSOR ;           -- Declare cursor variables     
	sql character varying;
	searchToken character varying;	
BEGIN  
	searchToken = replace(trim(BOTH ' ' FROM regexp_replace(lower(searchtext),'\s+',' ','g')), ' ', ':* & ') || ':*';	
	RAISE NOTICE 'Search tokens: %', searchToken ;
	sql := 'SELECT search.* , ts_rank_cd(search_phrase, q) AS rank ' ||
		'FROM search, to_tsquery(' || quote_literal(searchToken) || ') AS q  ' ||
		'WHERE (search_phrase @@ q) ORDER BY rank'   ;
		
	OPEN records FOR EXECUTE sql;
	--RAISE NOTICE '%', sql;
	RETURN records;
END;
$$;
ALTER FUNCTION public.search_records(searchtext character varying) OWNER TO ncuser;
CREATE FUNCTION trim_th_note() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
	max_length int DEFAULT 1000;
BEGIN
	IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE') THEN
		IF LENGTH(NEW.note) > max_length THEN
			NEW.note = substring(NEW.note FROM 0 FOR max_length);
			--RAISE EXCEPTION '%', NEW.note ;
		END IF;		
		RETURN NEW; 
	END IF;
END;
$$;
ALTER FUNCTION public.trim_th_note() OWNER TO ncuser;
CREATE FUNCTION update_all_custom_series() RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
	rec ns%rowtype;
	seq_name character varying;
BEGIN
	FOR rec IN
        SELECT * FROM NS ORDER BY refr
    LOOP
		seq_name := 'number_series_' || LOWER(rec.refr) || '_seq';
        RAISE NOTICE '%', 'PERFORM create_custom_series('|| seq_name ||', '|| rec.nnum ||',' || rec.dnum || ')';
        PERFORM create_custom_series(seq_name , rec.nnum , rec.dnum::INT );
    END LOOP;
END;
$$;
ALTER FUNCTION public.update_all_custom_series() OWNER TO ncuser;
CREATE FUNCTION update_bag_ref(jobid character varying) RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
	row we%rowtype;	
	fbgd character varying;
	sbgd character varying;
	rec RECORD;
BEGIN
	FOR row IN SELECT * FROM we WHERE csid = jobid AND bgid IS NOT NULL ORDER BY weid
    LOOP	
	SELECT bg.code AS fbgd, pg.code as sbgd INTO rec FROM bg LEFT JOIN bg as pg ON pg.bgid = bg.bref WHERE bg.bgid = row.bgid;
        RAISE NOTICE 'weid: % ; fbgd: % ; sbgd: %', row.weid, rec.fbgd, rec.sbgd;
        UPDATE we SET fbgd = rec.fbgd, sbgd = rec.sbgd WHERE we.weid = row.weid;
    END LOOP;	
END
$$;
ALTER FUNCTION public.update_bag_ref(jobid character varying) OWNER TO ncuser;
CREATE FUNCTION update_job_operation_key() RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE	
BEGIN
	UPDATE cs SET skey='DELETE' WHERE stat='D';
END;
$$;
ALTER FUNCTION public.update_job_operation_key() OWNER TO ncuser;
CREATE FUNCTION update_job_summery(consignmentid character varying) RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
	delivery_supplier character varying;
	transport_supplier character varying;	
BEGIN
	SELECT suid INTO delivery_supplier FROM jl WHERE CSID = consignmentId AND type='D' AND stat='D' ORDER BY seqn limit 1;		
	SELECT suid INTO transport_supplier FROM jl WHERE CSID = consignmentId AND type='T' AND stat='D' ORDER BY seqn limit 1;
	--RAISE NOTICE 'Delivery supplier %.', delivery_supplier;
	--RAISE NOTICE 'Transport supplier %.', transport_supplier;
	
	-- calculate delay in mins
	
	
	UPDATE cs set dsup = delivery_supplier, tsup = transport_supplier WHERE csid = consignmentId;
	
END
$$;
ALTER FUNCTION public.update_job_summery(consignmentid character varying) OWNER TO ncuser;
CREATE FUNCTION update_number_series(ref1 refcursor) RETURNS SETOF refcursor
    LANGUAGE plpgsql
    AS $$

DECLARE 
	
BEGIN
	OPEN ref1 for SELECT * FROM ns;
	RETURN NEXT ref1;
	
END;
$$;
ALTER FUNCTION public.update_number_series(ref1 refcursor) OWNER TO ncuser;
CREATE FUNCTION update_search_data(rec_type character varying, rec_id character varying, rec_name character varying, phrase tsvector) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
	rec_type = lower(rec_type);
	
	IF NOT EXISTS (SELECT * FROM search WHERE recordtype =  rec_type AND  recordid = rec_id) THEN
		--RAISE NOTICE 'Adding type: % record:%', rec_type, rec_id;
		INSERT INTO search (recordtype, recordid, recordname, search_phrase) VALUES (rec_type, rec_id, rec_name, phrase);
	ELSE
		--RAISE NOTICE 'Updating type: % record:%', rec_type, rec_id;
		UPDATE search SET recordtype = rec_type, recordid = rec_id, recordname=rec_name, search_phrase = phrase WHERE recordtype =  rec_type AND  recordid = rec_id;
	END IF;
	
END;
$$;
ALTER FUNCTION public.update_search_data(rec_type character varying, rec_id character varying, rec_name character varying, phrase tsvector) OWNER TO ncuser;
CREATE FUNCTION updatebatch() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$
DECLARE
	batchId VARCHAR;
	rec RECORD;
BEGIN
	IF (TG_OP = 'INSERT') THEN
		batchId := NEW.batch;
		SELECT COUNT(CASE WHEN ERROR = true THEN 1 ELSE null END) AS "success", COUNT(CASE WHEN ERROR = false THEN 1 ELSE null END) AS "failed" INTO rec FROM extra.im_jobreport WHERE batch=$1;
		UPDATE extra.im_jobbatch SET errors=rec.failed, success=rec.success  WHERE id= $1;
		RETURN NULL; 
	END IF;
END;
$_$;
ALTER FUNCTION public.updatebatch() OWNER TO ncuser;
CREATE FUNCTION updatepiecestatus(jobid character varying) RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
	we_skey character varying;
BEGIN
	SELECT skey INTO we_skey FROM CS WHERE CSID = jobId ;		
	IF we_skey IS NOT NULL THEN
		UPDATE WE SET skey = we_skey WHERE csid = jobId AND stat='L';
	END IF;
END;
$$;
ALTER FUNCTION public.updatepiecestatus(jobid character varying) OWNER TO ncuser;
SET search_path = extra, pg_catalog;
CREATE SEQUENCE batchstatus_serial_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE batchstatus_serial_seq OWNER TO ncuser;
CREATE SEQUENCE csvrow_serial_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE csvrow_serial_seq OWNER TO ncuser;
SET default_tablespace = '';
SET default_with_oids = false;
CREATE TABLE im_batchstatus (
    batch character varying,
    status character varying,
    version integer,
    id bigint NOT NULL
);
ALTER TABLE im_batchstatus OWNER TO ncuser;
CREATE TABLE im_csvrow (
    id integer NOT NULL,
    batch character varying(255) NOT NULL,
    data text,
    rowno integer NOT NULL,
    type character varying(255),
    version integer
);
ALTER TABLE im_csvrow OWNER TO ncuser;
CREATE TABLE im_jobbatch (
    id character varying(255) NOT NULL,
    username character varying(255),
    courier character varying(15),
    upload_date timestamp without time zone,
    finish_date timestamp without time zone,
    status character varying(512),
    is_update_jobs boolean,
    is_import_valids_only boolean,
    job_status character varying,
    uploaded_filepath character varying(1024),
    has_header boolean,
    email character varying(512),
    is_send_email boolean,
    service_action character varying(20),
    report_filepath character varying(1024),
    consignments text[],
    version smallint,
    job_count integer,
    archived boolean,
    errors integer DEFAULT 0,
    success integer DEFAULT 0,
    allow_delete boolean,
    notes text,
    source character varying(50),
    service_level_ids text,
    is_distribution_booking boolean,
    tag_id text,
    subject character varying,
    mailsent boolean,
    booking_screen_code character varying,
    converted_filepath character varying,
    template character varying
);
ALTER TABLE im_jobbatch OWNER TO ncuser;
COMMENT ON TABLE im_jobbatch IS 'Consignment batch for importer';
COMMENT ON COLUMN im_jobbatch.has_header IS 'Whether uploaded file has header row';
COMMENT ON COLUMN im_jobbatch.job_count IS 'Job row count';
CREATE TABLE im_jobreport (
    id integer NOT NULL,
    batch character varying(255) NOT NULL,
    csid character varying(25),
    details text,
    error boolean,
    rowno integer NOT NULL,
    version integer,
    jobrecord boolean DEFAULT true,
    address character varying(50),
    jobpiece character varying(10),
    jobweight numeric(19,3),
    jobreference text,
    jobnote text
);
ALTER TABLE im_jobreport OWNER TO ncuser;
CREATE SEQUENCE jobreport_serial_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE jobreport_serial_seq OWNER TO ncuser;
SET search_path = public, pg_catalog;
CREATE SEQUENCE aa_bookmark_seq
    START WITH 2053641430080946432
    INCREMENT BY 1
    MINVALUE 2053641430080946432
    MAXVALUE 2089670227099910143
    CACHE 1;
ALTER TABLE aa_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE ad_bookmark_seq
    START WITH 756604737398243584
    INCREMENT BY 1
    MINVALUE 756604737398243584
    MAXVALUE 792633534417207295
    CACHE 1;
ALTER TABLE ad_bookmark_seq OWNER TO ncuser;
CREATE TABLE ad (
    _bookmark_ bigint DEFAULT nextval('ad_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    adcd character varying,
    type character varying,
    cnam character varying,
    adr1 character varying,
    adr2 character varying,
    adr3 character varying,
    zpcd character varying,
    plid character varying,
    adr4 character varying,
    coid character varying,
    stat character varying(1),
    adno character varying,
    phon character varying,
    emal character varying,
    ctnm character varying,
    faxn character varying,
    adid character varying,
    clid character varying,
    orid character varying,
    dlst date,
    luis bigint,
    hits integer,
    "CTID" character varying,
    supf character varying(1),
    mobl character varying,
    tags character varying,
    bran character varying,
    adr5 character varying,
    hitr integer,
    latt numeric(10,8),
    lont numeric(10,8),
    vsno integer,
    search_phrase tsvector,
    cocd character varying,
    countryname character varying,
    plcd character varying,
    placealtname character varying,
    w3ws character varying
);
ALTER TABLE ad OWNER TO ncuser;
COMMENT ON TABLE ad IS 'Address File';
COMMENT ON COLUMN ad.crcd IS 'Courier Unique Code';
COMMENT ON COLUMN ad.adcd IS 'Address Code';
COMMENT ON COLUMN ad.type IS 'Address Type';
COMMENT ON COLUMN ad.cnam IS 'Company name';
COMMENT ON COLUMN ad.adr1 IS 'Address Line 1';
COMMENT ON COLUMN ad.adr2 IS 'Address Line 2';
COMMENT ON COLUMN ad.adr3 IS 'Address Line 3';
COMMENT ON COLUMN ad.zpcd IS 'Zip Code';
COMMENT ON COLUMN ad.plid IS 'Destination Id';
COMMENT ON COLUMN ad.adr4 IS 'State (Address 4)';
COMMENT ON COLUMN ad.coid IS 'Country Id';
COMMENT ON COLUMN ad.stat IS 'Record Status';
COMMENT ON COLUMN ad.adno IS 'Address Number';
COMMENT ON COLUMN ad.phon IS 'Phone No';
COMMENT ON COLUMN ad.emal IS 'Email';
COMMENT ON COLUMN ad.ctnm IS 'Contact';
COMMENT ON COLUMN ad.faxn IS 'Fax no';
COMMENT ON COLUMN ad.adid IS 'Address Id';
COMMENT ON COLUMN ad.clid IS 'Referer id';
COMMENT ON COLUMN ad.orid IS 'Original address Id';
COMMENT ON COLUMN ad.dlst IS 'Date last used';
COMMENT ON COLUMN ad.luis IS 'Date last used in inverse sequence';
COMMENT ON COLUMN ad.hits IS 'No of times used';
COMMENT ON COLUMN ad."CTID" IS 'Contact id';
COMMENT ON COLUMN ad.supf IS 'Address superseded flag';
COMMENT ON COLUMN ad.mobl IS 'Mobile no';
COMMENT ON COLUMN ad.tags IS 'Tags';
COMMENT ON COLUMN ad.bran IS 'Branch';
COMMENT ON COLUMN ad.adr5 IS 'Branch';
COMMENT ON COLUMN ad.hitr IS 'Hits reverse';
COMMENT ON COLUMN ad.latt IS 'Latitude';
COMMENT ON COLUMN ad.lont IS 'Longitude';
COMMENT ON COLUMN ad.vsno IS 'Version No.';
COMMENT ON COLUMN ad.w3ws IS 'Three words';
CREATE SEQUENCE an_bookmark_seq
    START WITH 864691128455135488
    INCREMENT BY 1
    MINVALUE 864691128455135488
    MAXVALUE 900719925474099199
    CACHE 1;
ALTER TABLE an_bookmark_seq OWNER TO ncuser;
CREATE TABLE an (
    _bookmark_ bigint DEFAULT nextval('an_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    date date,
    text character varying,
    usid character varying,
    stat character varying(1),
    vsno integer,
    anid character varying,
    crid character varying,
    bicu character varying,
    sicu character varying
);
ALTER TABLE an OWNER TO ncuser;
COMMENT ON TABLE an IS 'Announcement';
COMMENT ON COLUMN an.crcd IS 'Courier Code';
COMMENT ON COLUMN an.date IS 'Announcement Date';
COMMENT ON COLUMN an.text IS 'Announcement Text';
COMMENT ON COLUMN an.usid IS 'User Id';
COMMENT ON COLUMN an.stat IS 'Record Status';
COMMENT ON COLUMN an.vsno IS 'Version No.';
COMMENT ON COLUMN an.anid IS 'Announcement Id';
COMMENT ON COLUMN an.crid IS 'Office Id';
COMMENT ON COLUMN an.bicu IS 'Big Icon URL';
COMMENT ON COLUMN an.sicu IS 'Small Icon Url';
CREATE SEQUENCE ap_bookmark_seq
    START WITH 4179340454199820544
    INCREMENT BY 1
    MINVALUE 4179340454199820544
    MAXVALUE 4215369251218784255
    CACHE 1;
ALTER TABLE ap_bookmark_seq OWNER TO ncuser;
CREATE TABLE ap (
    _bookmark_ bigint DEFAULT nextval('ap_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    apid character varying,
    apcd character varying,
    "DESC" character varying,
    stat character varying(1),
    vsno integer,
    pcnm character varying,
    ocnm character varying(1),
    lcnm character varying,
    pad1 character varying,
    oad1 character varying(1),
    lad1 character varying,
    pad2 character varying,
    oad2 character varying(1),
    lad2 character varying,
    pad3 character varying,
    oad3 character varying(1),
    lad3 character varying,
    pad4 character varying,
    oad4 character varying(1),
    lad4 character varying,
    ppcd character varying,
    opcd character varying(1),
    lpcd character varying,
    pctn character varying,
    octn character varying(1),
    lctn character varying,
    peml character varying,
    oeml character varying(1),
    leml character varying,
    pphn character varying,
    ophn character varying(1),
    lphn character varying,
    pmbl character varying,
    ombl character varying(1),
    lmbl character varying,
    pfax character varying,
    ofax character varying(1),
    lfax character varying,
    pacd character varying,
    oacd character varying(1),
    lacd character varying,
    pbra character varying,
    obra character varying(1),
    lbra character varying,
    pad5 character varying,
    oad5 character varying(1),
    lad5 character varying,
    ppls character varying,
    ctmx integer,
    a3mx integer,
    mbmx integer,
    plmx integer,
    fxmx integer,
    cmmx integer,
    a1mx integer,
    cdmx integer,
    pcmx integer,
    a2mx integer,
    emmx integer,
    brmx integer,
    phmx integer,
    a5mx integer,
    a4mx integer,
    ow3w character varying(1),
    w3mx integer,
    pw3w character varying,
    lw3w character varying
);
ALTER TABLE ap OWNER TO ncuser;
COMMENT ON TABLE ap IS 'Address profile';
COMMENT ON COLUMN ap.crcd IS 'Courier unique code';
COMMENT ON COLUMN ap.apid IS 'Address code';
COMMENT ON COLUMN ap.apcd IS 'Profile name';
COMMENT ON COLUMN ap."DESC" IS 'Dscription';
COMMENT ON COLUMN ap.stat IS 'Status';
COMMENT ON COLUMN ap.vsno IS 'Version';
COMMENT ON COLUMN ap.pcnm IS 'Company prompt';
COMMENT ON COLUMN ap.ocnm IS 'Company option';
COMMENT ON COLUMN ap.lcnm IS 'Conpany list id';
COMMENT ON COLUMN ap.pad1 IS 'Address 1 prompt';
COMMENT ON COLUMN ap.oad1 IS 'Address 1 option';
COMMENT ON COLUMN ap.lad1 IS 'Address 1 list id';
COMMENT ON COLUMN ap.pad2 IS 'Address 2 prompt';
COMMENT ON COLUMN ap.oad2 IS 'Address 2 option';
COMMENT ON COLUMN ap.lad2 IS 'Address 2 list id';
COMMENT ON COLUMN ap.pad3 IS 'Address 3 prompt';
COMMENT ON COLUMN ap.oad3 IS 'Address 3 option';
COMMENT ON COLUMN ap.lad3 IS 'Address 3 list id';
COMMENT ON COLUMN ap.pad4 IS 'Address 4 prompt';
COMMENT ON COLUMN ap.oad4 IS 'Address 4 option';
COMMENT ON COLUMN ap.lad4 IS 'Address 4 list id';
COMMENT ON COLUMN ap.ppcd IS 'Postcode prompt';
COMMENT ON COLUMN ap.opcd IS 'Postcode option';
COMMENT ON COLUMN ap.lpcd IS 'Postcode list id';
COMMENT ON COLUMN ap.pctn IS 'Mailroom contact prompt';
COMMENT ON COLUMN ap.octn IS 'Mailroom contact option';
COMMENT ON COLUMN ap.lctn IS 'Mailroom contact list id';
COMMENT ON COLUMN ap.peml IS 'Email prompt';
COMMENT ON COLUMN ap.oeml IS 'Email option';
COMMENT ON COLUMN ap.leml IS 'Email list id';
COMMENT ON COLUMN ap.pphn IS 'Phone prompt';
COMMENT ON COLUMN ap.ophn IS 'Phone option';
COMMENT ON COLUMN ap.lphn IS 'Phone list id';
COMMENT ON COLUMN ap.pmbl IS 'Mobile prompt';
COMMENT ON COLUMN ap.ombl IS 'Mobile option';
COMMENT ON COLUMN ap.lmbl IS 'Mobile list id';
COMMENT ON COLUMN ap.pfax IS 'Fax prompt';
COMMENT ON COLUMN ap.ofax IS 'Fax option';
COMMENT ON COLUMN ap.lfax IS 'Fax list id';
COMMENT ON COLUMN ap.pacd IS 'Address code prompt';
COMMENT ON COLUMN ap.oacd IS 'Address code  option';
COMMENT ON COLUMN ap.lacd IS 'Address code list id';
COMMENT ON COLUMN ap.pbra IS 'Branch prompt';
COMMENT ON COLUMN ap.obra IS 'Branch option';
COMMENT ON COLUMN ap.lbra IS 'Branch code list id';
COMMENT ON COLUMN ap.pad5 IS 'Address 5 prompt';
COMMENT ON COLUMN ap.oad5 IS 'Address 5 option';
COMMENT ON COLUMN ap.lad5 IS 'Address 6 list id';
COMMENT ON COLUMN ap.ppls IS 'Place prompt';
COMMENT ON COLUMN ap.ctmx IS 'Contact maximum length';
COMMENT ON COLUMN ap.a3mx IS 'Address 3 maximum length';
COMMENT ON COLUMN ap.mbmx IS 'Mobile maximum length';
COMMENT ON COLUMN ap.plmx IS 'Place maximum length';
COMMENT ON COLUMN ap.fxmx IS 'Fax maximum length';
COMMENT ON COLUMN ap.cmmx IS 'Company maximum length';
COMMENT ON COLUMN ap.a1mx IS 'Address 1 maximum length';
COMMENT ON COLUMN ap.cdmx IS 'Code maximum length';
COMMENT ON COLUMN ap.pcmx IS 'Postcode maximum length';
COMMENT ON COLUMN ap.a2mx IS 'Address 2 maximum length';
COMMENT ON COLUMN ap.emmx IS 'Email maximum length';
COMMENT ON COLUMN ap.brmx IS 'Branch maximum length';
COMMENT ON COLUMN ap.phmx IS 'Phone maximum length';
COMMENT ON COLUMN ap.a5mx IS 'Address 5 maximum length';
COMMENT ON COLUMN ap.a4mx IS 'Address 4 maximum length';
COMMENT ON COLUMN ap.ow3w IS 'What 3 words option';
COMMENT ON COLUMN ap.w3mx IS 'What 3 words maximum length';
COMMENT ON COLUMN ap.pw3w IS 'What 3 words prompt';
COMMENT ON COLUMN ap.lw3w IS 'What 3 words list id';
CREATE SEQUENCE at_bookmark_seq
    START WITH 3062447746611937536
    INCREMENT BY 1
    MINVALUE 3062447746611937536
    MAXVALUE 3098476543630901247
    CACHE 1;
ALTER TABLE at_bookmark_seq OWNER TO ncuser;
CREATE TABLE at (
    _bookmark_ bigint DEFAULT nextval('at_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    arid character varying,
    suid character varying,
    cutm integer,
    retm integer,
    stat character varying(1),
    vsno integer
);
ALTER TABLE at OWNER TO ncuser;
COMMENT ON TABLE at IS 'Cut-off and Recovery Time File';
COMMENT ON COLUMN at.crcd IS 'Courier Code';
COMMENT ON COLUMN at.arid IS 'Airport Id';
COMMENT ON COLUMN at.suid IS 'Supplier Id';
COMMENT ON COLUMN at.cutm IS 'Cut-off Time';
COMMENT ON COLUMN at.retm IS 'Recovery Time';
COMMENT ON COLUMN at.stat IS 'Record Status';
COMMENT ON COLUMN at.vsno IS 'Version No.';
CREATE SEQUENCE au_bookmark_seq
    START WITH 936748722493063424
    INCREMENT BY 1
    MINVALUE 936748722493063424
    MAXVALUE 972777519512027135
    CACHE 1;
ALTER TABLE au_bookmark_seq OWNER TO ncuser;
CREATE TABLE au (
    _bookmark_ bigint DEFAULT nextval('au_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    aucd character varying,
    usid character varying,
    date date,
    "time" character varying,
    file character varying,
    fild character varying,
    oval character varying,
    nval character varying,
    note character varying,
    fldt character varying,
    recd character varying
);
ALTER TABLE au OWNER TO ncuser;
COMMENT ON TABLE au IS 'Audit File';
COMMENT ON COLUMN au.crcd IS 'Courier Code';
COMMENT ON COLUMN au.aucd IS 'Audit Code';
COMMENT ON COLUMN au.usid IS 'User Id';
COMMENT ON COLUMN au.date IS 'Date Stamp';
COMMENT ON COLUMN au."time" IS 'Time Stamp';
COMMENT ON COLUMN au.file IS 'Audit Event/Database File';
COMMENT ON COLUMN au.fild IS 'Field Name';
COMMENT ON COLUMN au.oval IS 'Old Value';
COMMENT ON COLUMN au.nval IS 'New Value';
COMMENT ON COLUMN au.note IS 'Additional Notes';
COMMENT ON COLUMN au.fldt IS 'Field Changed(description)';
COMMENT ON COLUMN au.recd IS 'Record(primary key value)';
CREATE SEQUENCE av_bookmark_seq
    START WITH 4143311657180856576
    INCREMENT BY 1
    MINVALUE 4143311657180856576
    MAXVALUE 4179340454199820287
    CACHE 1;
ALTER TABLE av_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE ba_bookmark_seq
    START WITH 2630102182384369920
    INCREMENT BY 1
    MINVALUE 2630102182384369920
    MAXVALUE 2666130979403333631
    CACHE 1;
ALTER TABLE ba_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE bc_bookmark_seq
    START WITH 2738188573441261824
    INCREMENT BY 1
    MINVALUE 2738188573441261824
    MAXVALUE 2774217370460225535
    CACHE 1;
ALTER TABLE bc_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE bd_bookmark_seq
    START WITH 828662331436171520
    INCREMENT BY 1
    MINVALUE 828662331436171520
    MAXVALUE 864691128455135231
    CACHE 1;
ALTER TABLE bd_bookmark_seq OWNER TO ncuser;
CREATE TABLE bg (
    bgid bigint NOT NULL,
    crcd character varying,
    code character varying NOT NULL,
    lgid character varying,
    mfid character varying,
    crdt timestamp without time zone,
    vsno integer,
    stat character varying,
    type character varying,
    bref bigint,
    lttx text
);
ALTER TABLE bg OWNER TO ncuser;
CREATE SEQUENCE bg_bgid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE bg_bgid_seq OWNER TO ncuser;
ALTER SEQUENCE bg_bgid_seq OWNED BY bg.bgid;
CREATE SEQUENCE bn_bookmark_seq
    START WITH 1981583836043018496
    INCREMENT BY 1
    MINVALUE 1981583836043018496
    MAXVALUE 2017612633061982207
    CACHE 1;
ALTER TABLE bn_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE bo_bookmark_seq
    START WITH 864691128455135488
    INCREMENT BY 1
    MINVALUE 864691128455135488
    MAXVALUE 900719925474099199
    CACHE 1;
ALTER TABLE bo_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE bp_bookmark_seq
    START WITH 792633534417207552
    INCREMENT BY 1
    MINVALUE 792633534417207552
    MAXVALUE 828662331436171263
    CACHE 1;
ALTER TABLE bp_bookmark_seq OWNER TO ncuser;
CREATE TABLE bp (
    _bookmark_ bigint DEFAULT nextval('bp_bookmark_seq'::regclass) NOT NULL,
    bpid character varying,
    crcd character varying,
    refr character varying,
    scrn character varying,
    bpcd character varying,
    "DESC" character varying,
    ishd character varying(1),
    ised character varying(1),
    iscm character varying(1),
    snam character varying,
    isau character varying(1),
    prfx character varying,
    leng integer,
    strt bigint,
    stat character varying(1),
    prio character varying,
    optn character varying,
    defl character varying,
    tabl character varying,
    fkey character varying,
    fval character varying,
    tval character varying,
    vfld character varying,
    dfld character varying,
    type character varying,
    lp01 character varying,
    lp02 character varying,
    lp03 character varying,
    lp04 character varying,
    lp05 character varying,
    lp06 character varying,
    lp07 character varying,
    lp08 character varying,
    lp09 character varying,
    lp10 character varying,
    lp11 character varying,
    lp12 character varying,
    lp13 character varying,
    lp14 character varying,
    lp15 character varying,
    vsno integer
);
ALTER TABLE bp OWNER TO ncuser;
COMMENT ON TABLE bp IS 'Booking preference';
COMMENT ON COLUMN bp.bpid IS 'Booking pref unique ID';
COMMENT ON COLUMN bp.crcd IS 'Courier ID';
COMMENT ON COLUMN bp.refr IS 'Reference ID';
COMMENT ON COLUMN bp.scrn IS 'Screen code';
COMMENT ON COLUMN bp.bpcd IS 'Preference code';
COMMENT ON COLUMN bp."DESC" IS 'Description of fields';
COMMENT ON COLUMN bp.ishd IS 'Is it hidden';
COMMENT ON COLUMN bp.ised IS 'Is it editable';
COMMENT ON COLUMN bp.iscm IS 'Is it compulsory';
COMMENT ON COLUMN bp.snam IS 'Name on screen ID';
COMMENT ON COLUMN bp.isau IS 'Is it automatic';
COMMENT ON COLUMN bp.prfx IS 'Prefix for auto number';
COMMENT ON COLUMN bp.leng IS 'Length of auto ref';
COMMENT ON COLUMN bp.strt IS 'Auto generated number start at';
COMMENT ON COLUMN bp.stat IS 'Status';
COMMENT ON COLUMN bp.prio IS 'Status';
COMMENT ON COLUMN bp.optn IS 'Options for maintenance';
COMMENT ON COLUMN bp.defl IS 'Default value';
COMMENT ON COLUMN bp.tabl IS 'From table';
COMMENT ON COLUMN bp.fkey IS 'From key';
COMMENT ON COLUMN bp.fval IS 'From value for key';
COMMENT ON COLUMN bp.tval IS 'To value for key';
COMMENT ON COLUMN bp.vfld IS 'Value field';
COMMENT ON COLUMN bp.dfld IS 'Display field';
COMMENT ON COLUMN bp.type IS 'Field type';
COMMENT ON COLUMN bp.lp01 IS 'Language prompt 1';
COMMENT ON COLUMN bp.lp02 IS 'Language prompt 2';
COMMENT ON COLUMN bp.lp03 IS 'Language prompt 3';
COMMENT ON COLUMN bp.lp04 IS 'Language prompt 4';
COMMENT ON COLUMN bp.lp05 IS 'Language prompt 5';
COMMENT ON COLUMN bp.lp06 IS 'Language prompt 6';
COMMENT ON COLUMN bp.lp07 IS 'Language prompt 7';
COMMENT ON COLUMN bp.lp08 IS 'Language prompt 8';
COMMENT ON COLUMN bp.lp09 IS 'Language prompt 9';
COMMENT ON COLUMN bp.lp10 IS 'Language prompt 10';
COMMENT ON COLUMN bp.lp11 IS 'Language prompt 11';
COMMENT ON COLUMN bp.lp12 IS 'Language prompt 12';
COMMENT ON COLUMN bp.lp13 IS 'Language prompt 13';
COMMENT ON COLUMN bp.lp14 IS 'Language prompt 14';
COMMENT ON COLUMN bp.lp15 IS 'Language prompt 15';
CREATE SEQUENCE br_bookmark_seq
    START WITH 900719925474099456
    INCREMENT BY 1
    MINVALUE 900719925474099456
    MAXVALUE 936748722493063167
    CACHE 1;
ALTER TABLE br_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE bt_bookmark_seq
    START WITH 3314649325744685312
    INCREMENT BY 1
    MINVALUE 3314649325744685312
    MAXVALUE 3350678122763649023
    CACHE 1;
ALTER TABLE bt_bookmark_seq OWNER TO ncuser;
CREATE TABLE bt (
    _bookmark_ bigint DEFAULT nextval('bt_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    btid character varying,
    type character varying,
    tgid character varying,
    rcid character varying,
    stat character varying(1),
    fl01 character varying,
    fl02 character varying,
    fl03 character varying,
    fl04 character varying,
    fl05 character varying,
    fl06 character varying,
    fl07 character varying,
    fl08 character varying,
    fl09 character varying,
    fl10 character varying,
    vsno integer
);
ALTER TABLE bt OWNER TO ncuser;
COMMENT ON TABLE bt IS 'Batch address relation';
COMMENT ON COLUMN bt.crcd IS 'Courier Code';
COMMENT ON COLUMN bt.btid IS 'Batch Id';
COMMENT ON COLUMN bt.type IS 'Type';
COMMENT ON COLUMN bt.tgid IS 'Tagi id';
COMMENT ON COLUMN bt.rcid IS 'Record id';
COMMENT ON COLUMN bt.stat IS 'Record Status';
COMMENT ON COLUMN bt.fl01 IS 'Value 1';
COMMENT ON COLUMN bt.fl02 IS 'Value 2';
COMMENT ON COLUMN bt.fl03 IS 'Value 3';
COMMENT ON COLUMN bt.fl04 IS 'Value 4';
COMMENT ON COLUMN bt.fl05 IS 'Value 5';
COMMENT ON COLUMN bt.fl06 IS 'Value 6';
COMMENT ON COLUMN bt.fl07 IS 'Value 7';
COMMENT ON COLUMN bt.fl08 IS 'Value 8';
COMMENT ON COLUMN bt.fl09 IS 'Value 9';
COMMENT ON COLUMN bt.fl10 IS 'Value 10';
COMMENT ON COLUMN bt.vsno IS 'Sersion';
CREATE SEQUENCE bx_bookmark_seq
    START WITH 2449958197289550080
    INCREMENT BY 1
    MINVALUE 2449958197289550080
    MAXVALUE 2485986994308513791
    CACHE 1;
ALTER TABLE bx_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE ca_bookmark_seq
    START WITH 2594073385365405952
    INCREMENT BY 1
    MINVALUE 2594073385365405952
    MAXVALUE 2630102182384369663
    CACHE 1;
ALTER TABLE ca_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE cb_bookmark_seq
    START WITH 3638908498915361024
    INCREMENT BY 1
    MINVALUE 3638908498915361024
    MAXVALUE 3674937295934324735
    CACHE 1;
ALTER TABLE cb_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE cc_bookmark_seq
    START WITH 1008806316530991360
    INCREMENT BY 1
    MINVALUE 1008806316530991360
    MAXVALUE 1044835113549955071
    CACHE 1;
ALTER TABLE cc_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE cd_bookmark_seq
    START WITH 4251398048237748480
    INCREMENT BY 1
    MINVALUE 4251398048237748480
    MAXVALUE 4287426845256712191
    CACHE 1;
ALTER TABLE cd_bookmark_seq OWNER TO ncuser;
CREATE TABLE cd (
    _bookmark_ bigint DEFAULT nextval('cd_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    cdid character varying,
    lscd character varying,
    clcd character varying,
    sequ character varying,
    udf1 character varying,
    udf2 character varying,
    udf3 character varying,
    udf4 character varying,
    udf5 character varying,
    udf6 character varying,
    udf7 character varying,
    stat character varying(1),
    lupt character varying,
    udf8 character varying,
    udf9 character varying,
    ud10 character varying,
    ud11 character varying,
    ud12 character varying,
    ud13 character varying,
    ud14 character varying,
    ud15 character varying
);
ALTER TABLE cd OWNER TO ncuser;
COMMENT ON TABLE cd IS 'Client data file';
COMMENT ON COLUMN cd.crcd IS 'Courier code';
COMMENT ON COLUMN cd.cdid IS 'Client data id';
COMMENT ON COLUMN cd.lscd IS 'List code';
COMMENT ON COLUMN cd.clcd IS 'Client code';
COMMENT ON COLUMN cd.sequ IS 'Display sequence';
COMMENT ON COLUMN cd.udf1 IS 'User Define value 1';
COMMENT ON COLUMN cd.udf2 IS 'User Define value 2';
COMMENT ON COLUMN cd.udf3 IS 'User Define value 3';
COMMENT ON COLUMN cd.udf4 IS 'User Define value 4';
COMMENT ON COLUMN cd.udf5 IS 'User Define value 5';
COMMENT ON COLUMN cd.udf6 IS 'User Define value 6';
COMMENT ON COLUMN cd.udf7 IS 'User Define value 7';
COMMENT ON COLUMN cd.stat IS 'Status';
COMMENT ON COLUMN cd.lupt IS 'Last Updated timestump';
COMMENT ON COLUMN cd.udf8 IS 'User Define value 8';
COMMENT ON COLUMN cd.udf9 IS 'User Define value 9';
COMMENT ON COLUMN cd.ud10 IS 'User Define value 10';
COMMENT ON COLUMN cd.ud11 IS 'User Define value 11';
COMMENT ON COLUMN cd.ud12 IS 'User Define value 12';
COMMENT ON COLUMN cd.ud13 IS 'User Define value 13';
COMMENT ON COLUMN cd.ud14 IS 'User Define value 14';
COMMENT ON COLUMN cd.ud15 IS 'User Define value 15';
CREATE SEQUENCE ch_bookmark_seq
    START WITH 1044835113549955328
    INCREMENT BY 1
    MINVALUE 1044835113549955328
    MAXVALUE 1080863910568919039
    CACHE 1;
ALTER TABLE ch_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE ci_bookmark_seq
    START WITH 972777519512027392
    INCREMENT BY 1
    MINVALUE 972777519512027392
    MAXVALUE 1008806316530991103
    CACHE 1;
ALTER TABLE ci_bookmark_seq OWNER TO ncuser;
CREATE TABLE ci (
    _bookmark_ bigint DEFAULT nextval('ci_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    ciid character varying,
    objt character varying,
    soid character varying,
    suid character varying,
    type integer,
    cuid character varying,
    crat numeric(12,6),
    qunt numeric(13,3),
    uprc numeric(12,3),
    uprs numeric(12,3),
    pric numeric(12,3),
    pris numeric(12,3),
    aisc numeric(12,3),
    aicc numeric(12,3),
    iref character varying,
    stat character varying(1),
    csid character varying,
    vsno integer,
    piid character varying,
    seqn character varying,
    crus character varying,
    crdt date,
    crtm character varying,
    dlus character varying,
    dldt date,
    dltm character varying,
    orjt character varying,
    lgid character varying,
    text character varying,
    lock character varying(1),
    bass character varying,
    meth character varying,
    unit character varying,
    ctyp character varying(1),
    styp character varying,
    stax character varying(1),
    lgcd character varying,
    fpid character varying,
    tpid character varying,
    plac character varying,
    rcnl character varying(1),
    reus character varying,
    redt date,
    retm character varying,
    chpc character varying(1),
    wegt numeric(8,3),
    tict numeric(8,3),
    wgrt numeric(8,3),
    twct numeric(8,3),
    calc character varying,
    occr numeric(12,3),
    ocsr numeric(12,3)
);
ALTER TABLE ci OWNER TO ncuser;
COMMENT ON TABLE ci IS 'Cost line';
COMMENT ON COLUMN ci.crcd IS 'Courier Code';
COMMENT ON COLUMN ci.ciid IS 'Job cost id';
COMMENT ON COLUMN ci.objt IS 'Object name';
COMMENT ON COLUMN ci.soid IS 'Source id';
COMMENT ON COLUMN ci.suid IS 'Supplier Id';
COMMENT ON COLUMN ci.type IS 'Record Type(1%%%%2%%%%3%%%%4%%%%5)';
COMMENT ON COLUMN ci.cuid IS 'Currency id';
COMMENT ON COLUMN ci.crat IS 'Currency rate';
COMMENT ON COLUMN ci.qunt IS 'Quantity';
COMMENT ON COLUMN ci.uprc IS 'Unit cost in courier cur';
COMMENT ON COLUMN ci.uprs IS 'Unit cost in sup curr';
COMMENT ON COLUMN ci.pric IS 'Total cost in courier curr';
COMMENT ON COLUMN ci.pris IS 'Total in supplier curr';
COMMENT ON COLUMN ci.aisc IS 'Actual inv Amount(In SU)';
COMMENT ON COLUMN ci.aicc IS 'Actual inv Amount(In CR)';
COMMENT ON COLUMN ci.iref IS 'Supplier Invoice Reference';
COMMENT ON COLUMN ci.stat IS 'Status';
COMMENT ON COLUMN ci.csid IS 'Job Id';
COMMENT ON COLUMN ci.vsno IS 'Version No.';
COMMENT ON COLUMN ci.piid IS 'Purchase Invoice ID';
COMMENT ON COLUMN ci.seqn IS 'Sequence Number';
COMMENT ON COLUMN ci.crus IS 'Created user id';
COMMENT ON COLUMN ci.crdt IS 'Created date';
COMMENT ON COLUMN ci.crtm IS 'Created time';
COMMENT ON COLUMN ci.dlus IS 'Deleted user id';
COMMENT ON COLUMN ci.dldt IS 'Deleted date';
COMMENT ON COLUMN ci.dltm IS 'Deleted time';
COMMENT ON COLUMN ci.orjt IS 'Original transaction id';
COMMENT ON COLUMN ci.lgid IS 'Leg id';
COMMENT ON COLUMN ci.text IS 'Text';
COMMENT ON COLUMN ci.lock IS 'Locked';
COMMENT ON COLUMN ci.bass IS 'Pricing base';
COMMENT ON COLUMN ci.meth IS 'Method of application';
COMMENT ON COLUMN ci.unit IS 'Mesurement unit';
COMMENT ON COLUMN ci.ctyp IS 'Cost type';
COMMENT ON COLUMN ci.styp IS 'Source type';
COMMENT ON COLUMN ci.stax IS 'Tax applicable falg';
COMMENT ON COLUMN ci.lgcd IS 'Leg code';
COMMENT ON COLUMN ci.fpid IS 'From place id';
COMMENT ON COLUMN ci.tpid IS 'To place id';
COMMENT ON COLUMN ci.plac IS 'Price line analysis code';
COMMENT ON COLUMN ci.rcnl IS 'Reconcilled flag';
COMMENT ON COLUMN ci.reus IS 'User Reconciled';
COMMENT ON COLUMN ci.redt IS 'Reconcile date';
COMMENT ON COLUMN ci.retm IS 'Reconcile time';
COMMENT ON COLUMN ci.chpc IS 'Charge by percent flag';
COMMENT ON COLUMN ci.wegt IS 'Weight';
COMMENT ON COLUMN ci.tict IS 'Total Item cost';
COMMENT ON COLUMN ci.wgrt IS 'Weight Rate';
COMMENT ON COLUMN ci.twct IS 'Total weight cost';
COMMENT ON COLUMN ci.calc IS 'Calculation details';
COMMENT ON COLUMN ci.occr IS 'Original cost in courier currency';
COMMENT ON COLUMN ci.ocsr IS 'Original cost in supplier currency';
CREATE SEQUENCE cl_bookmark_seq
    START WITH 1008806316530991360
    INCREMENT BY 1
    MINVALUE 1008806316530991360
    MAXVALUE 1044835113549955071
    CACHE 1;
ALTER TABLE cl_bookmark_seq OWNER TO ncuser;
CREATE TABLE cl (
    _bookmark_ bigint DEFAULT nextval('cl_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    clcd character varying,
    "DESC" character varying,
    ccrd character varying,
    stat character varying(1),
    txid character varying,
    wweb character varying(1),
    cssf character varying,
    vsno integer,
    clid character varying,
    grpp character varying(1),
    grpi character varying(1),
    mpcl character varying,
    micl character varying,
    fadt date,
    lsid character varying,
    acch character varying(1),
    infq character varying,
    ana1 character varying,
    ana2 character varying,
    ana3 character varying,
    jopi integer,
    pday character varying,
    vatr character varying,
    vatd character varying,
    sacd character varying,
    invn character varying,
    clcb character varying,
    podf character varying(4),
    tpri character varying,
    dclc character varying(1),
    bicu character varying,
    sicu character varying,
    idtl character varying(1),
    invf character varying,
    invt character varying,
    ivs1 character varying,
    iva1 character varying(1),
    ivs2 character varying,
    iva2 character varying(1),
    ivs3 character varying,
    iva3 character varying(1),
    ivdl character varying(1),
    ivsh character varying(1),
    shtm character varying,
    shtp character varying(1),
    shpr character varying(1),
    shem character varying(1),
    wcrf character varying(1),
    wsrv character varying(1),
    wcon character varying(1),
    wspi character varying(1),
    wcvl character varying(1),
    wnot character varying(1),
    weml character varying(1),
    wbku character varying(1),
    wspr character varying(1),
    wsn3 character varying(1),
    w3rd character varying(1),
    type character varying(1),
    slpr character varying,
    cadr character varying,
    iadr character varying,
    disc numeric(11,2),
    tsta character varying(1),
    wcss character varying,
    logo character varying,
    plog character varying,
    advs character varying(1),
    cltm character varying,
    awbt character varying,
    mfld character varying,
    pooc character varying,
    crvl character varying,
    cgrp character varying(1),
    pgrp character varying(1),
    igrp character varying(1),
    bgrp character varying(1),
    mbcl character varying,
    book character varying(1),
    grpb character varying(1),
    dtyp character varying(3),
    poem character varying(1),
    safp character varying(1),
    stop character varying(1),
    tags character varying,
    ccut character varying,
    pref character varying,
    c2vl character varying,
    crpr character varying,
    crls character varying,
    cuid character varying,
    dexc character varying(1),
    schd character varying(1),
    scmn integer,
    dttm character varying,
    dfmt character varying,
    dur1 character varying,
    ewgt integer,
    sund character varying(1),
    eeml character varying(1),
    afwd character varying(1),
    aofp character varying(1),
    lspf character varying(1),
    nsmc character varying(1),
    uccp character varying(1),
    emlt character varying,
    ieml character varying,
    c2rp character varying,
    c2rl character varying,
    stel character varying,
    sdco character varying(1),
    prlb character varying(1),
    boko character varying(1),
    podo character varying(1),
    dpot character varying,
    srem character varying,
    rdtm character varying,
    bifs character varying(1),
    eipf character varying(1),
    cram numeric(11,2),
    cra2 numeric(11,2),
    cdal character varying(1),
    sdal character varying(1),
    tdal character varying(1),
    fdal character varying(1),
    c3rp character varying,
    c3rl character varying,
    c3vl character varying,
    c4rp character varying,
    c4rl character varying,
    c4vl character varying,
    rpjb integer,
    rptm integer,
    hkey character varying,
    ofln character varying(1),
    card character varying(255),
    ivld character varying(255),
    wclv character varying(255),
    pft1 character varying,
    pft2 character varying,
    pft3 character varying,
    dadr character varying,
    lldt date,
    clmt numeric(10,2),
    ubam numeric(10,2),
    cthp numeric(10,2),
    lltm character varying,
    ultm character varying,
    uldt date,
    adue numeric(10,2),
    apik character varying,
    sudt date
);
ALTER TABLE cl OWNER TO ncuser;
COMMENT ON TABLE cl IS 'Customer File';
COMMENT ON COLUMN cl.crcd IS 'Courier Code';
COMMENT ON COLUMN cl.clcd IS 'Client code';
COMMENT ON COLUMN cl."DESC" IS 'Client Name';
COMMENT ON COLUMN cl.ccrd IS 'Collect card details';
COMMENT ON COLUMN cl.stat IS 'Record Status';
COMMENT ON COLUMN cl.txid IS 'Tax Id';
COMMENT ON COLUMN cl.wweb IS 'On-Line Quote';
COMMENT ON COLUMN cl.cssf IS 'Theme for Customer site';
COMMENT ON COLUMN cl.vsno IS 'Version No.';
COMMENT ON COLUMN cl.clid IS 'Customer Id';
COMMENT ON COLUMN cl.grpp IS 'Group price Chart Client';
COMMENT ON COLUMN cl.grpi IS 'Group Invoice Client';
COMMENT ON COLUMN cl.mpcl IS 'Master price client';
COMMENT ON COLUMN cl.micl IS 'Master Invoice Client';
COMMENT ON COLUMN cl.fadt IS 'First Active Date';
COMMENT ON COLUMN cl.lsid IS 'Sales Person Code';
COMMENT ON COLUMN cl.acch IS 'Account details changed';
COMMENT ON COLUMN cl.infq IS 'Invoice Group';
COMMENT ON COLUMN cl.ana1 IS 'Analysis 1';
COMMENT ON COLUMN cl.ana2 IS 'Analysis 2';
COMMENT ON COLUMN cl.ana3 IS 'Analysis 3';
COMMENT ON COLUMN cl.jopi IS 'No of jobs Per Invoice';
COMMENT ON COLUMN cl.pday IS 'PoD Day';
COMMENT ON COLUMN cl.vatr IS 'VAT Registration No.';
COMMENT ON COLUMN cl.vatd IS 'VAT Details';
COMMENT ON COLUMN cl.sacd IS 'Second Account Code';
COMMENT ON COLUMN cl.invn IS 'Invoice Number';
COMMENT ON COLUMN cl.clcb IS 'Client Closes By';
COMMENT ON COLUMN cl.podf IS 'POD format';
COMMENT ON COLUMN cl.tpri IS 'Tracking Priority';
COMMENT ON COLUMN cl.dclc IS 'Use Default Collection Add';
COMMENT ON COLUMN cl.bicu IS 'Big  Icon URL';
COMMENT ON COLUMN cl.sicu IS 'Small Icon URL';
COMMENT ON COLUMN cl.idtl IS 'Show Details';
COMMENT ON COLUMN cl.invf IS 'Invoice Frequency';
COMMENT ON COLUMN cl.invt IS 'Invoice Template';
COMMENT ON COLUMN cl.ivs1 IS 'Sequence 1';
COMMENT ON COLUMN cl.iva1 IS 'Action 1';
COMMENT ON COLUMN cl.ivs2 IS 'Sequence 2';
COMMENT ON COLUMN cl.iva2 IS 'Action 2';
COMMENT ON COLUMN cl.ivs3 IS 'Sequence 3';
COMMENT ON COLUMN cl.iva3 IS 'Action 3';
COMMENT ON COLUMN cl.ivdl IS 'Show Detail Lines';
COMMENT ON COLUMN cl.ivsh IS 'Schedule';
COMMENT ON COLUMN cl.shtm IS 'Schedule rpt template';
COMMENT ON COLUMN cl.shtp IS 'Schedule Type';
COMMENT ON COLUMN cl.shpr IS 'Print Schedule';
COMMENT ON COLUMN cl.shem IS 'E-mail Schedule';
COMMENT ON COLUMN cl.wcrf IS 'Web Option Cust Ref';
COMMENT ON COLUMN cl.wsrv IS 'Web Option Service';
COMMENT ON COLUMN cl.wcon IS 'Web Option Contents';
COMMENT ON COLUMN cl.wspi IS 'Web Option SPIN';
COMMENT ON COLUMN cl.wcvl IS 'Web Option Consignment Value';
COMMENT ON COLUMN cl.wnot IS 'Web Option Notes';
COMMENT ON COLUMN cl.weml IS 'Email Customer Service';
COMMENT ON COLUMN cl.wbku IS 'Booked By User';
COMMENT ON COLUMN cl.wspr IS 'Show price on CL online';
COMMENT ON COLUMN cl.wsn3 IS 'Show Supp and 3rd party on tra';
COMMENT ON COLUMN cl.w3rd IS '3rd Party Tracking';
COMMENT ON COLUMN cl.type IS 'Type of client';
COMMENT ON COLUMN cl.slpr IS 'Sales person';
COMMENT ON COLUMN cl.cadr IS 'Contact address';
COMMENT ON COLUMN cl.iadr IS 'Invoice address';
COMMENT ON COLUMN cl.disc IS 'Discount';
COMMENT ON COLUMN cl.tsta IS 'Tax status';
COMMENT ON COLUMN cl.wcss IS 'Client online css';
COMMENT ON COLUMN cl.logo IS 'Client online logo';
COMMENT ON COLUMN cl.plog IS 'Logo for Proforma';
COMMENT ON COLUMN cl.advs IS 'Address visible for all search';
COMMENT ON COLUMN cl.cltm IS 'Close at time';
COMMENT ON COLUMN cl.awbt IS 'AWB template';
COMMENT ON COLUMN cl.mfld IS 'Other mandatory field';
COMMENT ON COLUMN cl.pooc IS 'Parent online client optons';
COMMENT ON COLUMN cl.crvl IS 'Client ref validation';
COMMENT ON COLUMN cl.cgrp IS 'Is client group parent';
COMMENT ON COLUMN cl.pgrp IS 'Is client pricing group';
COMMENT ON COLUMN cl.igrp IS 'Is client invoice group';
COMMENT ON COLUMN cl.bgrp IS 'Is client booking group';
COMMENT ON COLUMN cl.mbcl IS 'Booking master client';
COMMENT ON COLUMN cl.book IS 'Booking allowed';
COMMENT ON COLUMN cl.grpb IS 'Use parent for booking';
COMMENT ON COLUMN cl.dtyp IS 'Discount type PCV-percent MUL for fixed';
COMMENT ON COLUMN cl.poem IS 'POD E-mail';
COMMENT ON COLUMN cl.safp IS 'Save address for parent';
COMMENT ON COLUMN cl.stop IS 'Stop credit';
COMMENT ON COLUMN cl.tags IS 'Tags';
COMMENT ON COLUMN cl.ccut IS 'Collection cut-off time';
COMMENT ON COLUMN cl.pref IS 'Preference screen id';
COMMENT ON COLUMN cl.c2vl IS '2nd ref validation';
COMMENT ON COLUMN cl.crpr IS 'Your ref prompt';
COMMENT ON COLUMN cl.crls IS 'Validation list';
COMMENT ON COLUMN cl.cuid IS 'Currency for invoice';
COMMENT ON COLUMN cl.dexc IS 'Data exchange enable';
COMMENT ON COLUMN cl.schd IS 'Shedule type';
COMMENT ON COLUMN cl.scmn IS 'Schedule minute';
COMMENT ON COLUMN cl.dttm IS 'Daily time';
COMMENT ON COLUMN cl.dfmt IS 'Data format';
COMMENT ON COLUMN cl.dur1 IS 'URL or emai 1';
COMMENT ON COLUMN cl.ewgt IS 'Email tracking weight';
COMMENT ON COLUMN cl.sund IS 'Allow Sunday dispatch';
COMMENT ON COLUMN cl.eeml IS 'Allow consignee email address?';
COMMENT ON COLUMN cl.afwd IS 'Forward-dating permitted?';
COMMENT ON COLUMN cl.aofp IS 'Addresses outside DX footprint permitted?';
COMMENT ON COLUMN cl.lspf IS 'Lack of site reference permitted?';
COMMENT ON COLUMN cl.nsmc IS 'Do not save address to master address list?';
COMMENT ON COLUMN cl.uccp IS 'Use client currency for pricing?';
COMMENT ON COLUMN cl.emlt IS 'Email template';
COMMENT ON COLUMN cl.ieml IS 'Invoice email address';
COMMENT ON COLUMN cl.c2rp IS 'Second ref prompt';
COMMENT ON COLUMN cl.c2rl IS 'Second ref validation list';
COMMENT ON COLUMN cl.stel IS 'Header section contact number';
COMMENT ON COLUMN cl.sdco IS 'Show default currency in client online?';
COMMENT ON COLUMN cl.prlb IS 'Print second label?';
COMMENT ON COLUMN cl.boko IS 'Booking confirmation?';
COMMENT ON COLUMN cl.podo IS 'POD notification?';
COMMENT ON COLUMN cl.dpot IS 'Depot code';
COMMENT ON COLUMN cl.srem IS 'Stock report recipient email';
COMMENT ON COLUMN cl.rdtm IS 'Ready at time';
COMMENT ON COLUMN cl.bifs IS 'Bypass IP filter for super user';
COMMENT ON COLUMN cl.eipf IS 'Enable IP filter during login';
COMMENT ON COLUMN cl.cram IS 'Credit amount left in courier currency';
COMMENT ON COLUMN cl.cra2 IS 'Credit amount left in client currency';
COMMENT ON COLUMN cl.cdal IS 'Display your ref as list?';
COMMENT ON COLUMN cl.sdal IS 'Display second ref as list?';
COMMENT ON COLUMN cl.tdal IS 'Display third ref as list?';
COMMENT ON COLUMN cl.fdal IS 'Display fourth ref as list?';
COMMENT ON COLUMN cl.c3rp IS 'Third ref prompt';
COMMENT ON COLUMN cl.c3rl IS 'Third ref validation list';
COMMENT ON COLUMN cl.c3vl IS 'Third ref validation';
COMMENT ON COLUMN cl.c4rp IS 'Fourth ref prompt';
COMMENT ON COLUMN cl.c4rl IS 'Frouth ref validation list';
COMMENT ON COLUMN cl.c4vl IS 'Fourth ref validation';
COMMENT ON COLUMN cl.rpjb IS 'Maximum report job';
COMMENT ON COLUMN cl.rptm IS 'Maximum report time in seconds';
COMMENT ON COLUMN cl.hkey IS 'Hash key value';
COMMENT ON COLUMN cl.ofln IS 'Is offline customer';
COMMENT ON COLUMN cl.pft1 IS 'Preferred template 1';
COMMENT ON COLUMN cl.pft2 IS 'Preferred template 2';
COMMENT ON COLUMN cl.pft3 IS 'Preferred template 3';
COMMENT ON COLUMN cl.dadr IS 'Default consignor address';
COMMENT ON COLUMN cl.lldt IS 'Lower limit reach date';
COMMENT ON COLUMN cl.clmt IS 'Credit limit';
COMMENT ON COLUMN cl.ubam IS 'Unbilled amount';
COMMENT ON COLUMN cl.cthp IS 'Credit threshold percentage';
COMMENT ON COLUMN cl.lltm IS 'Lower limit reach time';
COMMENT ON COLUMN cl.ultm IS 'Upper limit reach time';
COMMENT ON COLUMN cl.uldt IS 'Upper limit reach date';
COMMENT ON COLUMN cl.adue IS 'Amount due';
COMMENT ON COLUMN cl.apik IS 'API key';
CREATE SEQUENCE cm_bookmark_seq
    START WITH 2954361355555045632
    INCREMENT BY 1
    MINVALUE 2954361355555045632
    MAXVALUE 2990390152574009343
    CACHE 1;
ALTER TABLE cm_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE cn_bookmark_seq
    START WITH 1116892707587883264
    INCREMENT BY 1
    MINVALUE 1116892707587883264
    MAXVALUE 1152921504606846975
    CACHE 1;
ALTER TABLE cn_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE co_bookmark_seq
    START WITH 3386706919782613248
    INCREMENT BY 1
    MINVALUE 3386706919782613248
    MAXVALUE 3422735716801576959
    CACHE 1;
ALTER TABLE co_bookmark_seq OWNER TO ncuser;
CREATE TABLE co (
    _bookmark_ bigint DEFAULT nextval('co_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    cocd character varying,
    "DESC" character varying,
    cuid character varying,
    stat character varying(1),
    vsno integer,
    coid character varying,
    rfad character varying(1),
    ptxt character varying,
    vtxt character varying,
    nofr character varying(1),
    nosa character varying(1),
    nosu character varying(1),
    aaod character varying(1),
    acty character varying(1),
    apcd character varying(1),
    aprt character varying(1),
    aoth character varying(1),
    latt numeric(8,2),
    lont numeric(8,2),
    sicu character varying,
    trgn character varying(1),
    iseu character varying(1),
    hsta character varying(1),
    zpmn character varying(1),
    tags character varying,
    apid character varying,
    weig integer,
    dial character varying
);
ALTER TABLE co OWNER TO ncuser;
COMMENT ON TABLE co IS 'Country File';
COMMENT ON COLUMN co.crcd IS 'Courier Code';
COMMENT ON COLUMN co.cocd IS 'Country Short Name';
COMMENT ON COLUMN co."DESC" IS 'Country Name';
COMMENT ON COLUMN co.cuid IS 'Currency Id';
COMMENT ON COLUMN co.stat IS 'Record Status';
COMMENT ON COLUMN co.vsno IS 'Version No.';
COMMENT ON COLUMN co.coid IS 'Country Id';
COMMENT ON COLUMN co.rfad IS 'Full address required by cust';
COMMENT ON COLUMN co.ptxt IS 'Postal code validation';
COMMENT ON COLUMN co.vtxt IS 'VAT number validation';
COMMENT ON COLUMN co.nofr IS 'No deliveries on Friday';
COMMENT ON COLUMN co.nosa IS 'No deliveries on Saturday';
COMMENT ON COLUMN co.nosu IS 'No deliveries on Sunday';
COMMENT ON COLUMN co.aaod IS 'Allow other destination';
COMMENT ON COLUMN co.acty IS 'Allow cities';
COMMENT ON COLUMN co.apcd IS 'Allow post codes';
COMMENT ON COLUMN co.aprt IS 'Allow ports';
COMMENT ON COLUMN co.aoth IS 'Allow other';
COMMENT ON COLUMN co.latt IS 'Latitude';
COMMENT ON COLUMN co.lont IS 'Longitude';
COMMENT ON COLUMN co.sicu IS 'Small Icon Url';
COMMENT ON COLUMN co.trgn IS 'Tax region';
COMMENT ON COLUMN co.iseu IS 'No proforma required';
COMMENT ON COLUMN co.hsta IS 'Has state/province';
COMMENT ON COLUMN co.zpmn IS 'Zip code mandatory';
COMMENT ON COLUMN co.tags IS 'Tags';
COMMENT ON COLUMN co.apid IS 'Address profile';
COMMENT ON COLUMN co.weig IS 'Weight for display/search';
COMMENT ON COLUMN co.dial IS 'Dial code';
CREATE TABLE costs (
    cv_cpne numeric,
    cp01 numeric,
    cp02 numeric,
    cp03 numeric,
    cp04 numeric,
    cp05 numeric,
    cp06 numeric,
    cp07 numeric,
    cp08 numeric,
    cp09 numeric,
    cp10 numeric,
    cp11 numeric,
    cp12 numeric,
    cp13 numeric,
    cp14 numeric,
    cp15 numeric,
    cp16 numeric,
    cp17 numeric,
    cp18 numeric,
    cp19 numeric,
    cp20 numeric
);
ALTER TABLE costs OWNER TO ncuser;
CREATE SEQUENCE cp_bookmark_seq
    START WITH 1188950301625811200
    INCREMENT BY 1
    MINVALUE 1188950301625811200
    MAXVALUE 1224979098644774911
    CACHE 1;
ALTER TABLE cp_bookmark_seq OWNER TO ncuser;
CREATE TABLE cp (
    _bookmark_ bigint DEFAULT nextval('cp_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    cpid character varying,
    objt character varying,
    seqn character varying,
    soid character varying,
    orgt character varying,
    "DESC" character varying,
    qnty integer,
    pric numeric(11,2),
    tota numeric(11,2),
    crus character varying,
    crdt date,
    crtm character varying,
    stat character varying(1),
    dlus character varying,
    dldt date,
    dltm character varying,
    pri2 numeric(11,2),
    tot2 numeric(11,2),
    erat numeric(13,4),
    cmcd character varying
);
ALTER TABLE cp OWNER TO ncuser;
COMMENT ON TABLE cp IS 'Consignment proforma';
COMMENT ON COLUMN cp.crcd IS 'Courier code';
COMMENT ON COLUMN cp.cpid IS 'Unique id';
COMMENT ON COLUMN cp.objt IS 'Type of object';
COMMENT ON COLUMN cp.seqn IS 'Sequence number';
COMMENT ON COLUMN cp.soid IS 'Source object id';
COMMENT ON COLUMN cp.orgt IS 'Original transaction id';
COMMENT ON COLUMN cp."DESC" IS 'Description';
COMMENT ON COLUMN cp.qnty IS 'Quantity';
COMMENT ON COLUMN cp.pric IS 'Unit price';
COMMENT ON COLUMN cp.tota IS 'Total amount';
COMMENT ON COLUMN cp.crus IS 'Created user';
COMMENT ON COLUMN cp.crdt IS 'Created date';
COMMENT ON COLUMN cp.crtm IS 'Created time';
COMMENT ON COLUMN cp.stat IS 'Status';
COMMENT ON COLUMN cp.dlus IS 'Deleted user';
COMMENT ON COLUMN cp.dldt IS 'Deleted date';
COMMENT ON COLUMN cp.dltm IS 'Deleted time';
COMMENT ON COLUMN cp.pri2 IS 'Unit 2nd price';
COMMENT ON COLUMN cp.tot2 IS 'Total 2nd  amount';
COMMENT ON COLUMN cp.erat IS 'Exchange rate';
COMMENT ON COLUMN cp.cmcd IS 'Commodity code';
CREATE SEQUENCE cr_bookmark_seq
    START WITH 1080863910568919296
    INCREMENT BY 1
    MINVALUE 1080863910568919296
    MAXVALUE 1116892707587883007
    CACHE 1;
ALTER TABLE cr_bookmark_seq OWNER TO ncuser;
CREATE TABLE cr (
    _bookmark_ bigint DEFAULT nextval('cr_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    "DESC" character varying,
    logo character varying,
    cssf character varying,
    pass character varying,
    cuid character varying,
    disp integer,
    wunt character varying,
    uhwd character varying,
    lirn character varying,
    libn character varying,
    lawb character varying,
    bptp character varying(1),
    lusr character varying,
    lldt date,
    lltm character varying,
    rflo character varying,
    volf numeric(8,2),
    note character varying,
    stat character varying(1),
    lmfc character varying,
    lann character varying,
    rlsu character varying(1),
    laln character varying,
    podt date,
    linv character varying,
    sawb character varying(1),
    awbs character varying,
    llfn integer,
    lpfn integer,
    lmfn integer,
    lafn integer,
    lffn integer,
    drpl character varying,
    srpc character varying(1),
    emac character varying,
    vatn character varying,
    invt character varying,
    cstp character varying,
    slis character varying(1),
    slcs character varying(1),
    slas character varying(1),
    pabt character varying(1),
    pqte character varying(1),
    pboj character varying(1),
    ptnt character varying(1),
    pmft character varying(1),
    prpt character varying(1),
    pcnt character varying(1),
    wweb character varying(1),
    wstp character varying,
    wcss character varying,
    wcrd character varying,
    woqt character varying(1),
    wlgo character varying,
    wfm2 character varying,
    wfm3 character varying,
    wswb character varying(1),
    wfm1 character varying,
    wf1t character varying,
    wfcs character varying(1),
    wfmn character varying,
    wf3t character varying(1),
    wfp1 character varying,
    wfp2 character varying,
    wfp3 character varying,
    wf31 character varying(1),
    wbkh character varying,
    wqth character varying,
    wtth character varying,
    wmfh character varying,
    cznm character varying,
    iads character varying(1),
    rpsm character varying,
    nscd character varying,
    dpzn character varying,
    inoa numeric(11,2),
    inop numeric(6,0),
    ncrq character varying,
    nseb character varying,
    nceb character varying,
    shls character varying(1),
    icrf character varying(1),
    wmih character varying,
    wcuh character varying,
    vsno integer,
    crid character varying,
    dcss character varying,
    spas character varying,
    susr character varying,
    lsrv character varying,
    msrv character varying,
    vold numeric(8,2),
    home character varying,
    sfax character varying(1),
    crtz character varying,
    cawb character varying(1),
    pfxc character varying,
    pfxn character varying,
    scon character varying(1),
    sspi character varying(1),
    snot character varying(1),
    npro character varying(1),
    dapm character varying(1),
    slbl character varying,
    clbl character varying,
    pclc character varying,
    pcct character varying,
    psnm character varying,
    pstn character varying,
    pcpc character varying,
    pcld character varying,
    pdct character varying,
    pcnm character varying,
    pctn character varying,
    pdpc character varying,
    dfor character varying(1),
    sptx character varying(1),
    pssn character varying(1),
    weml character varying(1),
    bgcl character varying,
    dbgc character varying,
    dfgc character varying,
    fgcl character varying,
    lbgc character varying,
    lfgc character varying,
    rol1 character varying,
    rol2 character varying,
    rol3 character varying,
    rol4 character varying,
    rol5 character varying,
    rol6 character varying,
    rol0 character varying,
    mxad integer,
    nrad integer,
    nrpl integer,
    adid character varying,
    map1 character varying,
    map2 character varying,
    dunt character varying,
    shpw character varying(1),
    auts character varying(1),
    autr character varying(1),
    autc character varying(1),
    coid character varying,
    levl character varying,
    shal character varying(1),
    auto character varying(1),
    clcc character varying,
    agcc character varying,
    abar character varying(1),
    clgo character varying,
    shnd integer,
    ssvr character varying,
    dexc character varying(1),
    schd character varying(1),
    scmn integer,
    dttm character varying,
    dfmt character varying,
    emop character varying,
    ewgt integer,
    ufsl numeric(4,2)
);
ALTER TABLE cr OWNER TO ncuser;
COMMENT ON TABLE cr IS 'Courier Information File';
COMMENT ON COLUMN cr.crcd IS 'Courier Unique Code';
COMMENT ON COLUMN cr."DESC" IS 'Courier name';
COMMENT ON COLUMN cr.logo IS 'Courier Logo';
COMMENT ON COLUMN cr.cssf IS 'CSS File';
COMMENT ON COLUMN cr.pass IS 'Access Password';
COMMENT ON COLUMN cr.cuid IS 'Base Currency Id';
COMMENT ON COLUMN cr.disp IS 'No of Consignments to Display';
COMMENT ON COLUMN cr.wunt IS 'Unit of Weight';
COMMENT ON COLUMN cr.uhwd IS 'Unit of Length';
COMMENT ON COLUMN cr.lirn IS 'Last Internal Ref.';
COMMENT ON COLUMN cr.libn IS 'Last Invoice';
COMMENT ON COLUMN cr.lawb IS 'Last Air Way Bill';
COMMENT ON COLUMN cr.bptp IS 'Type of Batch Printing';
COMMENT ON COLUMN cr.lusr IS 'Last Login User';
COMMENT ON COLUMN cr.lldt IS 'Last Login date';
COMMENT ON COLUMN cr.lltm IS 'Last Login Time';
COMMENT ON COLUMN cr.rflo IS 'Report File Location';
COMMENT ON COLUMN cr.volf IS 'Volume Factor(all other)';
COMMENT ON COLUMN cr.note IS 'Additional Notes';
COMMENT ON COLUMN cr.stat IS 'Record Status';
COMMENT ON COLUMN cr.lmfc IS 'Last Manifest Code';
COMMENT ON COLUMN cr.lann IS 'Last Announcement number';
COMMENT ON COLUMN cr.rlsu IS 'Report Layout Scale Unit';
COMMENT ON COLUMN cr.laln IS 'Last Service Variant No';
COMMENT ON COLUMN cr.podt IS 'Last POD Print date';
COMMENT ON COLUMN cr.linv IS 'Last Invoice Number';
COMMENT ON COLUMN cr.sawb IS 'System Assign AWB';
COMMENT ON COLUMN cr.awbs IS 'Starting AWBS';
COMMENT ON COLUMN cr.llfn IS 'Last Location file Number';
COMMENT ON COLUMN cr.lpfn IS 'Last PA File Number';
COMMENT ON COLUMN cr.lmfn IS 'Last MF File Number';
COMMENT ON COLUMN cr.lafn IS 'Last Agent File Number';
COMMENT ON COLUMN cr.lffn IS 'Last Flight File Number';
COMMENT ON COLUMN cr.drpl IS 'Default Report Layout';
COMMENT ON COLUMN cr.srpc IS 'Service Related Price Chart';
COMMENT ON COLUMN cr.emac IS 'Email Account Name';
COMMENT ON COLUMN cr.vatn IS 'Vat Number';
COMMENT ON COLUMN cr.invt IS 'Invoice Report Text';
COMMENT ON COLUMN cr.cstp IS 'ACI Session Timeout Period';
COMMENT ON COLUMN cr.slis IS 'Show PBM on Internal Site';
COMMENT ON COLUMN cr.slcs IS 'Show PBM on Client Site';
COMMENT ON COLUMN cr.slas IS 'Show PBM on Agent Site';
COMMENT ON COLUMN cr.pabt IS 'Provide About (Menu)';
COMMENT ON COLUMN cr.pqte IS 'Provide Quote (Menu)';
COMMENT ON COLUMN cr.pboj IS 'Provide Book Job Menu';
COMMENT ON COLUMN cr.ptnt IS 'Provide Tracking (Menu)';
COMMENT ON COLUMN cr.pmft IS 'Provide Manifest (Menu)';
COMMENT ON COLUMN cr.prpt IS 'Provide Reports';
COMMENT ON COLUMN cr.pcnt IS 'Provide Contact Us (Menu)';
COMMENT ON COLUMN cr.wweb IS 'Web Site Required';
COMMENT ON COLUMN cr.wstp IS 'Web Session Timeout for CL-ONL';
COMMENT ON COLUMN cr.wcss IS 'Theme for Client online';
COMMENT ON COLUMN cr.wcrd IS 'Courier Short name (URL)';
COMMENT ON COLUMN cr.woqt IS 'Offer On-Line Quote';
COMMENT ON COLUMN cr.wlgo IS 'Browse For Logo';
COMMENT ON COLUMN cr.wfm2 IS 'Browse for fm2 background';
COMMENT ON COLUMN cr.wfm3 IS 'Browse for fm3 background';
COMMENT ON COLUMN cr.wswb IS 'Separate Web Site';
COMMENT ON COLUMN cr.wfm1 IS 'Browse for fm1 background';
COMMENT ON COLUMN cr.wf1t IS 'Frame 1 Text';
COMMENT ON COLUMN cr.wfcs IS 'Frame Within courier''s site';
COMMENT ON COLUMN cr.wfmn IS 'Frame name';
COMMENT ON COLUMN cr.wf3t IS 'Show text in fm3 of welcome pg';
COMMENT ON COLUMN cr.wfp1 IS 'Para 1';
COMMENT ON COLUMN cr.wfp2 IS 'Para 2';
COMMENT ON COLUMN cr.wfp3 IS 'Para 3';
COMMENT ON COLUMN cr.wf31 IS 'Logon pg in fm3 of welcome pg';
COMMENT ON COLUMN cr.wbkh IS 'Pg heading for Book a job';
COMMENT ON COLUMN cr.wqth IS 'Pg heading for Quote';
COMMENT ON COLUMN cr.wtth IS 'Page heading for T&T';
COMMENT ON COLUMN cr.wmfh IS 'Page Heading for Collection MF';
COMMENT ON COLUMN cr.cznm IS 'Custom Zone Name';
COMMENT ON COLUMN cr.iads IS 'Show Add Serv. in Invoice Rpt';
COMMENT ON COLUMN cr.rpsm IS 'Report summary fields';
COMMENT ON COLUMN cr.nscd IS 'Next Supplier Number';
COMMENT ON COLUMN cr.dpzn IS 'Default pick up zone';
COMMENT ON COLUMN cr.inoa IS 'Inv. Recon. Allowable overchar';
COMMENT ON COLUMN cr.inop IS 'Inv. Recon. Allowable overchar';
COMMENT ON COLUMN cr.ncrq IS 'Next Credit Request Ref.';
COMMENT ON COLUMN cr.nseb IS 'Next Sales Export Batch No.';
COMMENT ON COLUMN cr.nceb IS 'Next Cost Export Batch No.';
COMMENT ON COLUMN cr.shls IS 'Allow client online public tracking ';
COMMENT ON COLUMN cr.icrf IS 'Show Cust Ref on Invoice';
COMMENT ON COLUMN cr.wmih IS 'Page Heading for MIS';
COMMENT ON COLUMN cr.wcuh IS 'Page Heading for Contact US';
COMMENT ON COLUMN cr.vsno IS 'Version No.';
COMMENT ON COLUMN cr.crid IS 'Courier Information Id';
COMMENT ON COLUMN cr.dcss IS 'default Style-sheet';
COMMENT ON COLUMN cr.spas IS 'Super User Password';
COMMENT ON COLUMN cr.susr IS 'Super User';
COMMENT ON COLUMN cr.lsrv IS 'Live Server Name';
COMMENT ON COLUMN cr.msrv IS 'Mirror Server Name';
COMMENT ON COLUMN cr.vold IS 'Volume Factor(USA domestic)';
COMMENT ON COLUMN cr.home IS 'Home URL';
COMMENT ON COLUMN cr.sfax IS 'Send Fax';
COMMENT ON COLUMN cr.crtz IS 'Courier Time Zone';
COMMENT ON COLUMN cr.cawb IS 'Auto AWB no for client online';
COMMENT ON COLUMN cr.pfxc IS 'Prefix for client online AWB';
COMMENT ON COLUMN cr.pfxn IS 'Prefix for NC AWB';
COMMENT ON COLUMN cr.scon IS 'Save Contents from CL Online';
COMMENT ON COLUMN cr.sspi IS 'Save Special Ins. from Client';
COMMENT ON COLUMN cr.snot IS 'Show Note on Client Online';
COMMENT ON COLUMN cr.npro IS 'Not show proforma fields';
COMMENT ON COLUMN cr.dapm IS 'Use Default Address Prompt';
COMMENT ON COLUMN cr.slbl IS 'Collection Address Label';
COMMENT ON COLUMN cr.clbl IS 'Consignee Address Label';
COMMENT ON COLUMN cr.pclc IS 'Prompt for Collection Address';
COMMENT ON COLUMN cr.pcct IS 'Prompt for Collection Contact';
COMMENT ON COLUMN cr.psnm IS 'Prompt for Shipper Name';
COMMENT ON COLUMN cr.pstn IS 'Prompt for Shipper Town';
COMMENT ON COLUMN cr.pcpc IS 'Prompt for Collection Post Cod';
COMMENT ON COLUMN cr.pcld IS 'Prompt for Delivery Address';
COMMENT ON COLUMN cr.pdct IS 'Prompt for Delivery Contact';
COMMENT ON COLUMN cr.pcnm IS 'Prompt for Consignee Name';
COMMENT ON COLUMN cr.pctn IS 'Prompt for Consignee Town';
COMMENT ON COLUMN cr.pdpc IS 'Prompt for Delivery Post Code';
COMMENT ON COLUMN cr.dfor IS 'Date Format';
COMMENT ON COLUMN cr.sptx IS 'Show Text On POD Report';
COMMENT ON COLUMN cr.pssn IS 'Show Sender Details on PAMF';
COMMENT ON COLUMN cr.weml IS 'Web Option Email Cust Serv';
COMMENT ON COLUMN cr.bgcl IS 'Background colour';
COMMENT ON COLUMN cr.dbgc IS 'Background color for deleted d';
COMMENT ON COLUMN cr.dfgc IS 'Foreground color for deleted r';
COMMENT ON COLUMN cr.fgcl IS 'Foreground colour';
COMMENT ON COLUMN cr.lbgc IS 'Background color for live data';
COMMENT ON COLUMN cr.lfgc IS 'Foreground color for live data';
COMMENT ON COLUMN cr.rol1 IS 'Role 1 label';
COMMENT ON COLUMN cr.rol2 IS 'Role 2 label';
COMMENT ON COLUMN cr.rol3 IS 'Role 3 label';
COMMENT ON COLUMN cr.rol4 IS 'Role 4 label';
COMMENT ON COLUMN cr.rol5 IS 'Role 5 label';
COMMENT ON COLUMN cr.rol6 IS 'Role 6 label';
COMMENT ON COLUMN cr.rol0 IS 'Role 0 label';
COMMENT ON COLUMN cr.mxad IS 'Max age of addr to be searched';
COMMENT ON COLUMN cr.nrad IS 'Number of addr to be scanned';
COMMENT ON COLUMN cr.nrpl IS 'Number of places to be scanned';
COMMENT ON COLUMN cr.adid IS 'Courier address';
COMMENT ON COLUMN cr.map1 IS 'Map server 1 (country)';
COMMENT ON COLUMN cr.map2 IS 'Map server 2 (place)';
COMMENT ON COLUMN cr.dunt IS 'Unit of distance';
COMMENT ON COLUMN cr.auts IS 'Allow auto Pricing';
COMMENT ON COLUMN cr.autr IS 'Allow auto routing';
COMMENT ON COLUMN cr.autc IS 'ALlow auto costing';
COMMENT ON COLUMN cr.coid IS 'Courier base country';
COMMENT ON COLUMN cr.levl IS 'Log debug level';
COMMENT ON COLUMN cr.shal IS 'Show alert in popup';
COMMENT ON COLUMN cr.auto IS 'Turn on auto complete in booking screen';
COMMENT ON COLUMN cr.clcc IS 'Client email cc';
COMMENT ON COLUMN cr.agcc IS 'Agent email cc';
COMMENT ON COLUMN cr.abar IS 'Allow barcode';
COMMENT ON COLUMN cr.clgo IS 'Client online logo';
COMMENT ON COLUMN cr.shnd IS 'Show holiday notice in next n days';
COMMENT ON COLUMN cr.ssvr IS 'SMS server';
COMMENT ON COLUMN cr.dexc IS 'Data exchange enable';
COMMENT ON COLUMN cr.schd IS 'Shedule type';
COMMENT ON COLUMN cr.scmn IS 'Schedule minute';
COMMENT ON COLUMN cr.dttm IS 'Daily time';
COMMENT ON COLUMN cr.dfmt IS 'Data format';
COMMENT ON COLUMN cr.emop IS 'URL or emai 1';
COMMENT ON COLUMN cr.ewgt IS 'Email tracking weight';
COMMENT ON COLUMN cr.ufsl IS 'Upload file size limit';
CREATE SEQUENCE cs_bookmark_seq
    START WITH 1116892707587883264
    INCREMENT BY 1
    MINVALUE 1116892707587883264
    MAXVALUE 1152921504606846975
    CACHE 1;
ALTER TABLE cs_bookmark_seq OWNER TO ncuser;
CREATE TABLE cs (
    _bookmark_ bigint DEFAULT nextval('cs_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    csid character varying,
    circ character varying,
    pdid character varying,
    clid character varying,
    "CTID" character varying,
    cref character varying,
    crf2 character varying,
    tref character varying,
    hawb character varying,
    qref character varying,
    cadr character varying,
    ccnt character varying,
    rtim character varying,
    ctim character varying,
    dadr character varying,
    dcnt character varying,
    twgt numeric(12,3),
    vwgt numeric(12,3),
    cwgt numeric(12,3),
    titm integer,
    ctyp character varying,
    slid character varying,
    rtid character varying,
    dday date,
    dtim character varying,
    csvl numeric(18,6),
    ival numeric(18,6),
    spin character varying,
    note character varying,
    spcs character varying(2),
    rtcs character varying(2),
    cpcs character varying(2),
    nopg integer,
    cuid character varying,
    qtdt date,
    qttm character varying,
    qtus character varying,
    bkus character varying,
    bkdt date,
    bktm character varying,
    clus character varying,
    ccdt date,
    cctm character varying,
    podt date,
    potm character varying,
    pous character varying,
    dlto character varying,
    pode character varying(1),
    rcdt date,
    rctm character varying,
    rtdt date,
    wprc numeric(19,3),
    dsnt numeric(12,3),
    qprc numeric(19,3),
    ecpc numeric(19,3),
    acpc numeric(19,3),
    ecsp numeric(19,3),
    acsp numeric(19,3),
    espr numeric(19,3),
    vata numeric(19,3),
    inva numeric(19,3),
    pcus character varying,
    pcdt date,
    rtcu character varying,
    rtcd date,
    cpcu character varying,
    cpcd date,
    inbn character varying,
    indt date,
    sebn character varying,
    mfad character varying,
    rexp character varying,
    orgn character varying,
    regc character varying(1),
    rcdw character varying,
    qtok character varying(1),
    bkok character varying(1),
    ppok character varying(1),
    clok character varying(1),
    mfok character varying(1),
    riok character varying(1),
    dlok character varying(1),
    inok character varying(1),
    clon character varying(1),
    type character varying(1),
    prog integer,
    vsno integer,
    stat character varying(1),
    mfid character varying,
    dstn numeric(11,3),
    paid character varying,
    seok character varying(1),
    scok character varying(1),
    reok character varying(1),
    ceok character varying(1),
    ccok character varying(1),
    nact character varying,
    nadt date,
    natm character varying,
    cleg character varying,
    skey character varying,
    cnid character varying,
    nalg character varying,
    drid character varying,
    dvok character varying(1),
    rcok character varying(1),
    gros numeric(19,3),
    ntfy character varying(1),
    emad character varying,
    pnot character varying,
    pval numeric(12,3),
    cart character varying,
    card character varying,
    expr character varying,
    auth character varying,
    drnm character varying,
    days character varying,
    ppst character varying(1),
    ldal date,
    lcol date,
    mans character varying(1),
    manr character varying(1),
    manc character varying(1),
    rday date,
    sadr character varying,
    tcid character varying,
    tpid character varying,
    fcid character varying,
    fpid character varying,
    sign character varying,
    bvia character varying,
    tags character varying,
    rjid character varying,
    husr character varying,
    waok character varying(1),
    drdt date,
    drtm character varying,
    podr character varying(1),
    pkzf character varying,
    pdns character varying(1),
    eror character varying(1),
    icu1 character varying,
    icu2 character varying,
    qpr2 numeric(19,3),
    vat2 numeric(12,3),
    gro2 numeric(19,3),
    ina2 numeric(19,3),
    erat numeric(13,4),
    p2cr character varying,
    p2vl numeric(12,3),
    snto character varying,
    snby character varying,
    payr character varying,
    sent character varying(1),
    iva2 numeric(18,6),
    csv2 numeric(18,6),
    dsn2 numeric(12,3),
    def1 numeric(12,3),
    def2 numeric(12,3),
    titl character varying,
    init character varying,
    ud01 character varying,
    ud02 character varying,
    ud03 character varying,
    ud04 character varying,
    ud05 character varying,
    ud06 character varying,
    ud07 character varying,
    ud08 character varying,
    door character varying,
    dadt date,
    datm character varying,
    csvt character varying,
    tmid character varying,
    ldat character varying,
    ud09 character varying,
    ud10 character varying,
    ud11 character varying,
    ud12 character varying,
    ud13 character varying,
    ud14 character varying,
    ud15 character varying,
    crf3 character varying,
    crf4 character varying,
    btyp character varying(1),
    bsta character varying,
    crnj character varying(1),
    schl character varying,
    bmet character varying,
    chng character varying(1),
    ud16 character varying,
    ud17 character varying,
    ud18 character varying,
    ud19 character varying,
    ud20 character varying,
    drsn character varying,
    ana1 character varying,
    ana2 character varying,
    csv3 numeric(18,6),
    iva3 numeric(18,6),
    csvr numeric(14,6),
    ivar numeric(14,6),
    csvc character varying,
    ivac character varying,
    pckn integer,
    cins character varying,
    dpot character varying,
    ptyp character varying,
    ptrk character varying(1),
    splt character varying(1),
    fref character varying,
    exlr character varying(1),
    bcsr character varying,
    catp character varying,
    datp character varying,
    cbld character varying,
    cflr character varying,
    clcn character varying,
    dbld character varying,
    dflr character varying,
    dlcn character varying,
    sphn character varying,
    smob character varying,
    seml character varying,
    rpos character varying,
    rphn character varying,
    reml character varying,
    cllg character varying,
    kpi1 character varying,
    lstr character varying,
    lsdt date,
    lstm character varying,
    cmlr character varying(1),
    tcnt integer,
    cmfp character varying(1),
    cndt date,
    cntm character varying,
    bcid character varying(255),
    expp numeric(19,3),
    chep numeric(19,3),
    cpsn character varying,
    epsn character varying,
    occr numeric(12,3),
    purf character varying,
    ocsr numeric(12,3),
    ikey character varying,
    dsup character varying,
    tsup character varying,
    dlay bigint,
    swgt numeric(12,3),
    csbk character varying(1),
    dins character varying,
    sttp character varying,
    rttp character varying,
    stin character varying,
    rtin character varying,
    dact character varying,
    sdtp character varying,
    ishz character varying,
    strm character varying,
    spur character varying,
    pdnf character varying,
    spar character varying,
    slc1 character varying,
    asvd character varying,
    sigr character varying,
    btpt character varying,
    dpct character varying,
    dpps character varying,
    lblt character varying,
    fslp numeric(19,3)
);
ALTER TABLE cs OWNER TO ncuser;
COMMENT ON TABLE cs IS 'Consignment File';
COMMENT ON COLUMN cs.crcd IS 'Courier Code';
COMMENT ON COLUMN cs.csid IS 'Unique Id';
COMMENT ON COLUMN cs.circ IS 'Internal Ref. Code';
COMMENT ON COLUMN cs.pdid IS 'Product Id';
COMMENT ON COLUMN cs.clid IS 'Customer Id';
COMMENT ON COLUMN cs."CTID" IS 'Bookby contact id';
COMMENT ON COLUMN cs.cref IS 'Client references';
COMMENT ON COLUMN cs.crf2 IS 'Client 2nd references';
COMMENT ON COLUMN cs.tref IS 'Client 3rd references';
COMMENT ON COLUMN cs.hawb IS 'House airway bill no';
COMMENT ON COLUMN cs.qref IS 'Quote references';
COMMENT ON COLUMN cs.cadr IS 'Collection address id';
COMMENT ON COLUMN cs.ccnt IS 'Collect from';
COMMENT ON COLUMN cs.rtim IS 'Ready at time';
COMMENT ON COLUMN cs.ctim IS 'Close time';
COMMENT ON COLUMN cs.dadr IS 'Destination address';
COMMENT ON COLUMN cs.dcnt IS 'Delivery contact';
COMMENT ON COLUMN cs.twgt IS 'Total weight (dead weight)';
COMMENT ON COLUMN cs.vwgt IS 'Volumetric weight';
COMMENT ON COLUMN cs.cwgt IS 'Chargeable weight';
COMMENT ON COLUMN cs.titm IS 'Total number of units';
COMMENT ON COLUMN cs.ctyp IS 'Contents type';
COMMENT ON COLUMN cs.slid IS 'Service level id';
COMMENT ON COLUMN cs.rtid IS 'Route id';
COMMENT ON COLUMN cs.dday IS 'Expected delivery day';
COMMENT ON COLUMN cs.dtim IS 'Expected delivery time';
COMMENT ON COLUMN cs.csvl IS 'Consignment value';
COMMENT ON COLUMN cs.ival IS 'Insurance value';
COMMENT ON COLUMN cs.spin IS 'Special instruction';
COMMENT ON COLUMN cs.note IS 'Note';
COMMENT ON COLUMN cs.spcs IS 'Sales price status';
COMMENT ON COLUMN cs.rtcs IS 'Route detail status';
COMMENT ON COLUMN cs.cpcs IS 'Cost price status';
COMMENT ON COLUMN cs.nopg IS 'Total No. of Packages';
COMMENT ON COLUMN cs.cuid IS 'Consignment Value Currency';
COMMENT ON COLUMN cs.qtdt IS 'Quote Date';
COMMENT ON COLUMN cs.qttm IS 'Quote Time';
COMMENT ON COLUMN cs.qtus IS 'Quote By User';
COMMENT ON COLUMN cs.bkus IS 'Booked by User';
COMMENT ON COLUMN cs.bkdt IS 'Booking Date';
COMMENT ON COLUMN cs.bktm IS 'Booking Time';
COMMENT ON COLUMN cs.clus IS 'Collected By User';
COMMENT ON COLUMN cs.ccdt IS 'Collection Date';
COMMENT ON COLUMN cs.cctm IS 'Collection Time';
COMMENT ON COLUMN cs.podt IS 'Proof of Delivery Date';
COMMENT ON COLUMN cs.potm IS 'Proof of Delivery Time';
COMMENT ON COLUMN cs.pous IS 'POD by User';
COMMENT ON COLUMN cs.dlto IS 'Delivered To';
COMMENT ON COLUMN cs.pode IS 'POD Email Sent';
COMMENT ON COLUMN cs.rcdt IS 'Requested Collection Date';
COMMENT ON COLUMN cs.rctm IS 'Requested Collection Time';
COMMENT ON COLUMN cs.rtdt IS 'RTI Date';
COMMENT ON COLUMN cs.wprc IS 'Weighted Price(basic price)';
COMMENT ON COLUMN cs.dsnt IS 'Discount amount';
COMMENT ON COLUMN cs.qprc IS 'Quote Price (list price)';
COMMENT ON COLUMN cs.ecpc IS 'Estimated Cost Price';
COMMENT ON COLUMN cs.acpc IS 'Actual Cost Price';
COMMENT ON COLUMN cs.ecsp IS 'Estimated Consignment Profit';
COMMENT ON COLUMN cs.acsp IS 'Actual Consignment Profit';
COMMENT ON COLUMN cs.espr IS 'Summation of Extra Sales Price';
COMMENT ON COLUMN cs.vata IS 'Vat Amount';
COMMENT ON COLUMN cs.inva IS 'Invoice Amount';
COMMENT ON COLUMN cs.pcus IS 'Price Checked by user';
COMMENT ON COLUMN cs.pcdt IS 'Price Checked Date';
COMMENT ON COLUMN cs.rtcu IS 'Route Checked by user';
COMMENT ON COLUMN cs.rtcd IS 'Route Checked Date';
COMMENT ON COLUMN cs.cpcu IS 'Cost Checked by user';
COMMENT ON COLUMN cs.cpcd IS 'Cost Checked Date';
COMMENT ON COLUMN cs.inbn IS 'Invoice Batch Number';
COMMENT ON COLUMN cs.indt IS 'Invoice Date';
COMMENT ON COLUMN cs.sebn IS 'Sales Export Batch No.';
COMMENT ON COLUMN cs.mfad IS 'Name and Address of Manufacture';
COMMENT ON COLUMN cs.rexp IS 'Reason for Export';
COMMENT ON COLUMN cs.orgn IS 'Origin';
COMMENT ON COLUMN cs.regc IS 'Regular Consignment Flag';
COMMENT ON COLUMN cs.rcdw IS 'Regular Consignment days of we';
COMMENT ON COLUMN cs.qtok IS 'Quoted';
COMMENT ON COLUMN cs.bkok IS 'Booked';
COMMENT ON COLUMN cs.ppok IS 'Pre-Printed';
COMMENT ON COLUMN cs.clok IS 'Collected';
COMMENT ON COLUMN cs.mfok IS 'Good to go';
COMMENT ON COLUMN cs.riok IS 'Ready to Invoice';
COMMENT ON COLUMN cs.dlok IS 'Delivered';
COMMENT ON COLUMN cs.inok IS 'Invoiced';
COMMENT ON COLUMN cs.clon IS 'Booked From Client Online';
COMMENT ON COLUMN cs.type IS 'Job Type';
COMMENT ON COLUMN cs.prog IS 'Progress Status';
COMMENT ON COLUMN cs.vsno IS 'Version No.';
COMMENT ON COLUMN cs.stat IS 'Record Status';
COMMENT ON COLUMN cs.mfid IS 'Manifest Header ID';
COMMENT ON COLUMN cs.dstn IS 'Distance';
COMMENT ON COLUMN cs.paid IS 'Price allocation id';
COMMENT ON COLUMN cs.seok IS 'Sales price entered';
COMMENT ON COLUMN cs.scok IS 'Sales price checked';
COMMENT ON COLUMN cs.reok IS 'Route entered';
COMMENT ON COLUMN cs.ceok IS 'Cost price entered';
COMMENT ON COLUMN cs.ccok IS 'Cost price checked';
COMMENT ON COLUMN cs.nact IS 'Next action';
COMMENT ON COLUMN cs.nadt IS 'Next action date';
COMMENT ON COLUMN cs.natm IS 'Next action time';
COMMENT ON COLUMN cs.cleg IS 'Current leg';
COMMENT ON COLUMN cs.skey IS 'Search key element';
COMMENT ON COLUMN cs.cnid IS 'Contents';
COMMENT ON COLUMN cs.nalg IS 'Next action leg';
COMMENT ON COLUMN cs.drid IS 'Driver id';
COMMENT ON COLUMN cs.dvok IS 'Driver assigned';
COMMENT ON COLUMN cs.rcok IS 'Package received';
COMMENT ON COLUMN cs.gros IS 'Gross amount';
COMMENT ON COLUMN cs.ntfy IS 'Notify on delivery by email';
COMMENT ON COLUMN cs.emad IS 'Email';
COMMENT ON COLUMN cs.pnot IS 'Proforma note';
COMMENT ON COLUMN cs.pval IS 'Proforma value';
COMMENT ON COLUMN cs.cart IS 'Credit card name';
COMMENT ON COLUMN cs.card IS 'Credit card number';
COMMENT ON COLUMN cs.expr IS 'Card expiry date';
COMMENT ON COLUMN cs.auth IS 'Card authorization code';
COMMENT ON COLUMN cs.drnm IS 'Driver name';
COMMENT ON COLUMN cs.days IS 'Collection days';
COMMENT ON COLUMN cs.ppst IS 'Permission to post';
COMMENT ON COLUMN cs.ldal IS 'Last driver allocation';
COMMENT ON COLUMN cs.lcol IS 'Last collection date';
COMMENT ON COLUMN cs.mans IS 'Manual intervention on pricing';
COMMENT ON COLUMN cs.manr IS 'Manual intervention on routing';
COMMENT ON COLUMN cs.manc IS 'Manual intervention on costing';
COMMENT ON COLUMN cs.rday IS 'Ready at date';
COMMENT ON COLUMN cs.sadr IS 'Sender details';
COMMENT ON COLUMN cs.tcid IS 'Destination country id';
COMMENT ON COLUMN cs.tpid IS 'Destination place id';
COMMENT ON COLUMN cs.fcid IS 'Pickup country id';
COMMENT ON COLUMN cs.fpid IS 'Pickup place id';
COMMENT ON COLUMN cs.sign IS 'Signature image';
COMMENT ON COLUMN cs.bvia IS 'Book via';
COMMENT ON COLUMN cs.tags IS 'Tags';
COMMENT ON COLUMN cs.rjid IS 'Revrse of job id';
COMMENT ON COLUMN cs.husr IS 'Handheld user';
COMMENT ON COLUMN cs.waok IS 'Waiting acceptance';
COMMENT ON COLUMN cs.drdt IS 'Delivery required by date';
COMMENT ON COLUMN cs.drtm IS 'Delivery required by time';
COMMENT ON COLUMN cs.podr IS 'PoD required';
COMMENT ON COLUMN cs.pkzf IS 'Package format';
COMMENT ON COLUMN cs.pdns IS 'PoD Notification Sent';
COMMENT ON COLUMN cs.eror IS 'Error Exists';
COMMENT ON COLUMN cs.icu1 IS 'Invoice currency 1';
COMMENT ON COLUMN cs.icu2 IS 'Invoice currency 2';
COMMENT ON COLUMN cs.qpr2 IS 'Quote price in 2nd currency';
COMMENT ON COLUMN cs.vat2 IS 'VAT amont in 2nd currency';
COMMENT ON COLUMN cs.gro2 IS 'Gross amount in 2nd currency';
COMMENT ON COLUMN cs.ina2 IS 'Invoice amount in 2nd currency';
COMMENT ON COLUMN cs.erat IS 'Exchange rate';
COMMENT ON COLUMN cs.p2cr IS 'Proforma currency';
COMMENT ON COLUMN cs.p2vl IS 'Proforma 2nd value';
COMMENT ON COLUMN cs.snto IS 'Sent to';
COMMENT ON COLUMN cs.snby IS 'Send by';
COMMENT ON COLUMN cs.payr IS 'Payor';
COMMENT ON COLUMN cs.sent IS 'Job sent to supplier';
COMMENT ON COLUMN cs.iva2 IS 'Insurance in client currency';
COMMENT ON COLUMN cs.csv2 IS 'Consignment value in client currency';
COMMENT ON COLUMN cs.dsn2 IS 'Discount in client currency';
COMMENT ON COLUMN cs.def1 IS 'User defined field 1';
COMMENT ON COLUMN cs.def2 IS 'User defined field 2';
COMMENT ON COLUMN cs.titl IS 'Title of the recipient';
COMMENT ON COLUMN cs.init IS 'Initial of the recipient';
COMMENT ON COLUMN cs.ud01 IS 'User defined data 1';
COMMENT ON COLUMN cs.ud02 IS 'User defined data 2';
COMMENT ON COLUMN cs.ud03 IS 'User defined data 3';
COMMENT ON COLUMN cs.ud04 IS 'User defined data 4';
COMMENT ON COLUMN cs.ud05 IS 'User defined data 5';
COMMENT ON COLUMN cs.ud06 IS 'User defined data 6';
COMMENT ON COLUMN cs.ud07 IS 'User defined data 7';
COMMENT ON COLUMN cs.ud08 IS 'User defined data 8';
COMMENT ON COLUMN cs.door IS 'Door image';
COMMENT ON COLUMN cs.dadt IS 'Deliver after date';
COMMENT ON COLUMN cs.datm IS 'Deliver after time';
COMMENT ON COLUMN cs.csvt IS 'Consignment value text';
COMMENT ON COLUMN cs.tmid IS 'TMID of most recent transm';
COMMENT ON COLUMN cs.ldat IS 'Last driver allocation time';
COMMENT ON COLUMN cs.ud09 IS 'User defined data 9';
COMMENT ON COLUMN cs.ud10 IS 'User defined data 10';
COMMENT ON COLUMN cs.ud11 IS 'User defined data 11';
COMMENT ON COLUMN cs.ud12 IS 'User defined data 12';
COMMENT ON COLUMN cs.ud13 IS 'User defined data 13';
COMMENT ON COLUMN cs.ud14 IS 'User defined data 14';
COMMENT ON COLUMN cs.ud15 IS 'User defined data 15';
COMMENT ON COLUMN cs.crf3 IS 'Client 3rd references';
COMMENT ON COLUMN cs.crf4 IS 'Client 4th references';
COMMENT ON COLUMN cs.btyp IS 'Booking type';
COMMENT ON COLUMN cs.bsta IS 'Booking status';
COMMENT ON COLUMN cs.crnj IS 'Create new job?';
COMMENT ON COLUMN cs.schl IS 'Schedule number for 7 days';
COMMENT ON COLUMN cs.bmet IS 'Booking method';
COMMENT ON COLUMN cs.chng IS 'Job changed';
COMMENT ON COLUMN cs.ud16 IS 'User defined data 16';
COMMENT ON COLUMN cs.ud17 IS 'User defined data 17';
COMMENT ON COLUMN cs.ud18 IS 'User defined data 18';
COMMENT ON COLUMN cs.ud19 IS 'User defined data 19';
COMMENT ON COLUMN cs.ud20 IS 'User defined data 20';
COMMENT ON COLUMN cs.drsn IS 'Reason Id for the delay';
COMMENT ON COLUMN cs.ana1 IS 'Job analysys code 1';
COMMENT ON COLUMN cs.ana2 IS 'Job analysys code 2';
COMMENT ON COLUMN cs.csv3 IS 'Consignment value in third currency';
COMMENT ON COLUMN cs.iva3 IS 'Insurance value in third currency';
COMMENT ON COLUMN cs.csvr IS 'Consignment value exchange rate';
COMMENT ON COLUMN cs.ivar IS 'Insurance value exchange rate';
COMMENT ON COLUMN cs.csvc IS 'Consignment currency';
COMMENT ON COLUMN cs.ivac IS 'Insurance currency';
COMMENT ON COLUMN cs.pckn IS 'Package number';
COMMENT ON COLUMN cs.cins IS 'Collection instruction';
COMMENT ON COLUMN cs.dpot IS 'Depot';
COMMENT ON COLUMN cs.ptyp IS 'Payment type';
COMMENT ON COLUMN cs.ptrk IS 'Allow piece tracking';
COMMENT ON COLUMN cs.splt IS 'Split jobs';
COMMENT ON COLUMN cs.fref IS '4th Party Reference';
COMMENT ON COLUMN cs.exlr IS 'EXport License Required';
COMMENT ON COLUMN cs.bcsr IS 'Booking screen code';
COMMENT ON COLUMN cs.catp IS 'Cllection address Type';
COMMENT ON COLUMN cs.datp IS 'Delivery address Type';
COMMENT ON COLUMN cs.cbld IS 'Collection building';
COMMENT ON COLUMN cs.cflr IS 'Collection floor';
COMMENT ON COLUMN cs.clcn IS 'Collection location';
COMMENT ON COLUMN cs.dbld IS 'Delivery building';
COMMENT ON COLUMN cs.dflr IS 'Delivery floor';
COMMENT ON COLUMN cs.dlcn IS 'Delivery location';
COMMENT ON COLUMN cs.sphn IS 'Sender phone';
COMMENT ON COLUMN cs.smob IS 'Sender mobile';
COMMENT ON COLUMN cs.seml IS 'Sender mobile';
COMMENT ON COLUMN cs.rpos IS 'Recipient position';
COMMENT ON COLUMN cs.rphn IS 'Recipient phone';
COMMENT ON COLUMN cs.reml IS 'Recipient email';
COMMENT ON COLUMN cs.cllg IS 'Collection leg';
COMMENT ON COLUMN cs.kpi1 IS 'KPI data';
COMMENT ON COLUMN cs.lstr IS 'Last tracking text';
COMMENT ON COLUMN cs.lsdt IS 'Last Tracking Date';
COMMENT ON COLUMN cs.lstm IS 'Last Tracking Time';
COMMENT ON COLUMN cs.cmlr IS 'Collect from mailroom';
COMMENT ON COLUMN cs.tcnt IS 'Job transmission attempt counter';
COMMENT ON COLUMN cs.cmfp IS 'Client manifest printed';
COMMENT ON COLUMN cs.cndt IS 'Booking cancellation date';
COMMENT ON COLUMN cs.cntm IS 'Booking cancellation time';
COMMENT ON COLUMN cs.bcid IS 'Job import batch id';
COMMENT ON COLUMN cs.expp IS 'Expensive sales price';
COMMENT ON COLUMN cs.chep IS 'Cheapest sales price';
COMMENT ON COLUMN cs.cpsn IS 'Cheapest service name';
COMMENT ON COLUMN cs.epsn IS 'Expensive service name';
COMMENT ON COLUMN cs.occr IS 'Original cost in courier currency';
COMMENT ON COLUMN cs.purf IS 'Client Pick up references';
COMMENT ON COLUMN cs.ocsr IS 'Original cost in supplier currency';
COMMENT ON COLUMN cs.ikey IS 'Invoiceing key';
COMMENT ON COLUMN cs.swgt IS 'Supplier weight';
COMMENT ON COLUMN cs.csbk IS 'Cancel supplier booking';
CREATE SEQUENCE ct_bookmark_seq
    START WITH 1152921504606847232
    INCREMENT BY 1
    MINVALUE 1152921504606847232
    MAXVALUE 1188950301625810943
    CACHE 1;
ALTER TABLE ct_bookmark_seq OWNER TO ncuser;
CREATE TABLE ct (
    _bookmark_ bigint DEFAULT nextval('ct_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    "CTID" character varying,
    type character varying,
    "DESC" character varying,
    meth character varying,
    detl character varying,
    stat character varying(1),
    sequ integer,
    refr character varying,
    role character varying,
    met2 character varying,
    met3 character varying,
    det2 character varying,
    det3 character varying,
    pobn character varying,
    pobv character varying,
    met4 character varying,
    det4 character varying,
    laid character varying,
    lnam character varying,
    emad character varying,
    onlg character varying(1),
    usnm character varying,
    pass character varying,
    asub character varying(1),
    pdat date,
    autp character varying(1),
    pref character varying,
    supu character varying(1),
    rdef character varying,
    amet character varying,
    stpw character varying(1),
    teln character varying,
    mobl character varying,
    dept character varying,
    buld character varying,
    dtxp integer,
    lncd character varying,
    flor character varying,
    locn character varying,
    ref1 character varying,
    ref2 character varying,
    ref3 character varying,
    ref4 character varying,
    cldt date,
    cltm character varying,
    lldt date,
    lltm character varying,
    opls character varying,
    dfcl character varying,
    dflt character varying,
    cfdp character varying,
    mfdp character varying,
    rfdp character varying,
    vsno integer,
    plt2 character varying,
    plt3 character varying,
    plt1 character varying,
    drpt character varying,
    salt character varying,
    apik character varying
);
ALTER TABLE ct OWNER TO ncuser;
COMMENT ON TABLE ct IS 'Contact File';
COMMENT ON COLUMN ct.crcd IS 'Courier Unique Code';
COMMENT ON COLUMN ct."CTID" IS 'Contact Id';
COMMENT ON COLUMN ct.type IS 'Contact Type';
COMMENT ON COLUMN ct."DESC" IS 'Contact Name';
COMMENT ON COLUMN ct.meth IS 'Contact Method';
COMMENT ON COLUMN ct.detl IS 'Contact Details';
COMMENT ON COLUMN ct.stat IS 'Record Status';
COMMENT ON COLUMN ct.sequ IS 'Sequence No.(Contact Methods)';
COMMENT ON COLUMN ct.refr IS 'Contact reference';
COMMENT ON COLUMN ct.role IS 'Role';
COMMENT ON COLUMN ct.met2 IS 'Contact method 2';
COMMENT ON COLUMN ct.met3 IS 'Contact method 3';
COMMENT ON COLUMN ct.det2 IS 'Contact details 2';
COMMENT ON COLUMN ct.det3 IS 'Contact details 3';
COMMENT ON COLUMN ct.pobn IS 'Parent Object Name';
COMMENT ON COLUMN ct.pobv IS 'Parent Object Value';
COMMENT ON COLUMN ct.met4 IS 'Contact method 4';
COMMENT ON COLUMN ct.det4 IS 'Contact details 4';
COMMENT ON COLUMN ct.laid IS 'Last address entered by this user';
COMMENT ON COLUMN ct.lnam IS 'Last name(auto genarated)';
COMMENT ON COLUMN ct.emad IS 'Email address';
COMMENT ON COLUMN ct.onlg IS 'Allow online login';
COMMENT ON COLUMN ct.usnm IS 'User name';
COMMENT ON COLUMN ct.pass IS 'Password';
COMMENT ON COLUMN ct.asub IS 'Allow bookings to other group members';
COMMENT ON COLUMN ct.pdat IS 'Password change date';
COMMENT ON COLUMN ct.autp IS 'Auto generate password';
COMMENT ON COLUMN ct.pref IS 'Profile screen';
COMMENT ON COLUMN ct.supu IS 'Super user';
COMMENT ON COLUMN ct.rdef IS 'Report default for CLON';
COMMENT ON COLUMN ct.amet IS 'Contact added method flag';
COMMENT ON COLUMN ct.stpw IS 'Strong password support';
COMMENT ON COLUMN ct.teln IS 'Phone number';
COMMENT ON COLUMN ct.mobl IS 'Mobile number';
COMMENT ON COLUMN ct.dept IS 'Department';
COMMENT ON COLUMN ct.buld IS 'Building code';
COMMENT ON COLUMN ct.dtxp IS 'Day to expire password';
COMMENT ON COLUMN ct.lncd IS 'Language id (1-15)';
COMMENT ON COLUMN ct.flor IS 'Floor';
COMMENT ON COLUMN ct.locn IS 'Location';
COMMENT ON COLUMN ct.ref1 IS 'References 1';
COMMENT ON COLUMN ct.ref2 IS 'References 2';
COMMENT ON COLUMN ct.ref3 IS 'References 3';
COMMENT ON COLUMN ct.ref4 IS 'References 4';
COMMENT ON COLUMN ct.cldt IS 'Current login date';
COMMENT ON COLUMN ct.cltm IS 'Current login time';
COMMENT ON COLUMN ct.lldt IS 'Last login date';
COMMENT ON COLUMN ct.lltm IS 'Last login time';
COMMENT ON COLUMN ct.opls IS 'Last password list';
COMMENT ON COLUMN ct.dfcl IS 'Default client id';
COMMENT ON COLUMN ct.dflt IS 'Default label template';
COMMENT ON COLUMN ct.cfdp IS 'Controller default filters';
COMMENT ON COLUMN ct.mfdp IS 'Manifest default filters';
COMMENT ON COLUMN ct.rfdp IS 'Report default filters';
COMMENT ON COLUMN ct.vsno IS 'Version No';
COMMENT ON COLUMN ct.plt2 IS 'Priority label type 2';
COMMENT ON COLUMN ct.plt3 IS 'Priority label type 3';
COMMENT ON COLUMN ct.plt1 IS 'Priority label type 1';
COMMENT ON COLUMN ct.drpt IS 'Default report template';
COMMENT ON COLUMN ct.apik IS 'API key';
CREATE SEQUENCE cu_bookmark_seq
    START WITH 2630102182384369920
    INCREMENT BY 1
    MINVALUE 2630102182384369920
    MAXVALUE 2666130979403333631
    CACHE 1;
ALTER TABLE cu_bookmark_seq OWNER TO ncuser;
CREATE TABLE cu (
    _bookmark_ bigint DEFAULT nextval('cu_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    cucd character varying,
    cuid character varying,
    "DESC" character varying,
    stat character varying(1),
    vsno integer,
    bicu character varying,
    sicu character varying
);
ALTER TABLE cu OWNER TO ncuser;
COMMENT ON TABLE cu IS 'Currency File';
COMMENT ON COLUMN cu.crcd IS 'Courier Code';
COMMENT ON COLUMN cu.cucd IS 'Currency Code';
COMMENT ON COLUMN cu.cuid IS 'Currency Id';
COMMENT ON COLUMN cu."DESC" IS 'Currency Name';
COMMENT ON COLUMN cu.stat IS 'Record Status';
COMMENT ON COLUMN cu.vsno IS 'Version No';
COMMENT ON COLUMN cu.bicu IS 'Big Icon URL';
COMMENT ON COLUMN cu.sicu IS 'Small Icon URL';
CREATE SEQUENCE cv_bookmark_seq
    START WITH 4107282860161892608
    INCREMENT BY 1
    MINVALUE 4107282860161892608
    MAXVALUE 4143311657180856319
    CACHE 1;
ALTER TABLE cv_bookmark_seq OWNER TO ncuser;
CREATE TABLE cv (
    _bookmark_ bigint DEFAULT nextval('cv_bookmark_seq'::regclass) NOT NULL,
    csid character varying,
    frco character varying,
    frpl character varying,
    toco character varying,
    topl character varying,
    coag character varying,
    ftls character varying,
    ftln character varying,
    ftma character varying,
    deag character varying,
    spne numeric(11,2),
    spa1 numeric(11,2),
    spa2 numeric(11,2),
    spa3 numeric(11,2),
    spa4 numeric(11,2),
    cpne numeric(11,2),
    cpa1 numeric(11,2),
    cpa2 numeric(11,2),
    cpa3 numeric(11,2),
    cpa4 numeric(11,2),
    prne numeric(11,2),
    nasu character varying,
    nale character varying,
    nama character varying,
    jobs integer,
    cstd character varying(1),
    prpc numeric(11,2),
    prma numeric(11,2),
    crcd character varying,
    dtim integer,
    fdln character varying,
    rtdt character varying,
    rtat character varying,
    ftdt character varying,
    ftat character varying,
    fddt character varying,
    fdat character varying,
    nd01 date,
    nt01 character varying,
    nx01 character varying,
    nd02 date,
    nt02 character varying,
    nx02 character varying,
    nd03 date,
    nt03 character varying,
    nx03 character varying,
    nd04 date,
    nt04 character varying,
    nx04 character varying,
    nd05 date,
    nt05 character varying,
    nx05 character varying,
    nd06 date,
    nt06 character varying,
    nx06 character varying,
    nd07 date,
    nt07 character varying,
    nx07 character varying,
    nd08 date,
    nt08 character varying,
    nx08 character varying,
    nd09 date,
    nt09 character varying,
    nx09 character varying,
    nd10 date,
    nt10 character varying,
    nx10 character varying,
    nd11 date,
    nt11 character varying,
    nx11 character varying,
    nd12 date,
    nt12 character varying,
    nx12 character varying,
    nd13 date,
    nt13 character varying,
    nx13 character varying,
    nd14 date,
    nt14 character varying,
    nx14 character varying,
    nd15 date,
    nt15 character varying,
    nx15 character varying,
    nm01 numeric(11,2),
    nm02 numeric(11,2),
    nm03 numeric(11,2),
    nm04 numeric(11,2),
    nm05 numeric(11,2),
    nm06 numeric(11,2),
    nm07 numeric(11,2),
    nm08 numeric(11,2),
    nm09 numeric(11,2),
    nm10 numeric(11,2),
    nm11 numeric(11,2),
    nm12 numeric(11,2),
    nm13 numeric(11,2),
    nm14 numeric(11,2),
    nm15 numeric(11,2),
    regc character varying(1),
    dely integer,
    cdly integer,
    ocol character varying,
    thtm character varying,
    thdt date,
    smlp character varying,
    thtx character varying,
    tpcl character varying(1),
    odel character varying,
    sp01 numeric(11,2),
    sp02 numeric(11,2),
    sp03 numeric(11,2),
    sp04 numeric(11,2),
    sp05 numeric(11,2),
    sp06 numeric(11,2),
    sp07 numeric(11,2),
    sp08 numeric(11,2),
    sp09 numeric(11,2),
    sp10 numeric(11,2),
    sp11 numeric(11,2),
    sp12 numeric(11,2),
    sp13 numeric(11,2),
    sp14 numeric(11,2),
    sp15 numeric(11,2),
    sp16 numeric(11,2),
    sp17 numeric(11,2),
    sp18 numeric(11,2),
    sp19 numeric(11,2),
    sp20 numeric(11,2),
    cp01 numeric(11,2),
    cp02 numeric(11,2),
    cp03 numeric(11,2),
    cp04 numeric(11,2),
    cp05 numeric(11,2),
    cp06 numeric(11,2),
    cp07 numeric(11,2),
    cp08 numeric(11,2),
    cp09 numeric(11,2),
    cp10 numeric(11,2),
    cp11 numeric(11,2),
    cp12 numeric(11,2),
    cp13 numeric(11,2),
    cp14 numeric(11,2),
    cp15 numeric(11,2),
    cp16 numeric(11,2),
    cp17 numeric(11,2),
    cp18 numeric(11,2),
    cp19 numeric(11,2),
    cp20 numeric(11,2),
    nm16 integer,
    nm17 integer,
    nm18 integer,
    nm19 integer,
    nm20 integer,
    nd16 date,
    nd17 date,
    nd18 date,
    nd19 date,
    nd20 date,
    nx16 character varying,
    nx17 character varying,
    nx18 character varying,
    nx19 character varying,
    nx20 character varying,
    nt16 character varying,
    nt17 character varying,
    nt18 character varying,
    nt19 character varying,
    nt20 character varying,
    stat character varying,
    vsno integer
);
ALTER TABLE cv OWNER TO ncuser;
COMMENT ON TABLE cv IS 'Consignment virtual field';
COMMENT ON COLUMN cv.csid IS 'Consignment code';
COMMENT ON COLUMN cv.frco IS 'Origin country';
COMMENT ON COLUMN cv.frpl IS 'Origin place';
COMMENT ON COLUMN cv.toco IS 'Destination country';
COMMENT ON COLUMN cv.topl IS 'Destination place';
COMMENT ON COLUMN cv.coag IS 'Collection agent';
COMMENT ON COLUMN cv.ftls IS 'First transport leg supplier';
COMMENT ON COLUMN cv.ftln IS 'First transport leg name';
COMMENT ON COLUMN cv.ftma IS 'First transport leg MAWB';
COMMENT ON COLUMN cv.deag IS 'Delivery agent';
COMMENT ON COLUMN cv.spne IS 'Sale price';
COMMENT ON COLUMN cv.spa1 IS 'Sale price analysis 1';
COMMENT ON COLUMN cv.spa2 IS 'Sale price analysis 2';
COMMENT ON COLUMN cv.spa3 IS 'Sale price analysis 3';
COMMENT ON COLUMN cv.spa4 IS 'Sale price analysis 4';
COMMENT ON COLUMN cv.cpne IS 'Cost price';
COMMENT ON COLUMN cv.cpa1 IS 'Cost price analysis 1';
COMMENT ON COLUMN cv.cpa2 IS 'Cost price analysis 2';
COMMENT ON COLUMN cv.cpa3 IS 'Cost price analysis 3';
COMMENT ON COLUMN cv.cpa4 IS 'Cost price analysis 4';
COMMENT ON COLUMN cv.prne IS 'Job profit';
COMMENT ON COLUMN cv.nasu IS 'Next action';
COMMENT ON COLUMN cv.nale IS 'Next action leg';
COMMENT ON COLUMN cv.nama IS 'Next action manifest name';
COMMENT ON COLUMN cv.jobs IS 'No of jobs';
COMMENT ON COLUMN cv.cstd IS 'Fully costed';
COMMENT ON COLUMN cv.prpc IS 'Profit percent';
COMMENT ON COLUMN cv.prma IS 'Profit margin';
COMMENT ON COLUMN cv.crcd IS 'Courier code';
COMMENT ON COLUMN cv.dtim IS 'Delivery time in minute';
COMMENT ON COLUMN cv.fdln IS 'First delivery leg name';
COMMENT ON COLUMN cv.rtdt IS 'Route departure time (first weekday)';
COMMENT ON COLUMN cv.rtat IS 'Route arrival time (first weekday)';
COMMENT ON COLUMN cv.ftdt IS 'First transport leg departure time';
COMMENT ON COLUMN cv.ftat IS 'First transport leg arrival time';
COMMENT ON COLUMN cv.fddt IS 'First delivery leg departure time';
COMMENT ON COLUMN cv.fdat IS 'First delivery leg arrival time';
COMMENT ON COLUMN cv.nd01 IS 'Next action 1 date';
COMMENT ON COLUMN cv.nt01 IS 'Next action 1 time';
COMMENT ON COLUMN cv.nx01 IS 'Next action 1 name';
COMMENT ON COLUMN cv.nd02 IS 'Next action 2 date';
COMMENT ON COLUMN cv.nt02 IS 'Next action 2 time';
COMMENT ON COLUMN cv.nx02 IS 'Next action 2 name';
COMMENT ON COLUMN cv.nd03 IS 'Next action 3 date';
COMMENT ON COLUMN cv.nt03 IS 'Next action 3 time';
COMMENT ON COLUMN cv.nx03 IS 'Next action 3 name';
COMMENT ON COLUMN cv.nd04 IS 'Next action 4 date';
COMMENT ON COLUMN cv.nt04 IS 'Next action 4 time';
COMMENT ON COLUMN cv.nx04 IS 'Next action 4 name';
COMMENT ON COLUMN cv.nd05 IS 'Next action 5 date';
COMMENT ON COLUMN cv.nt05 IS 'Next action 5 time';
COMMENT ON COLUMN cv.nx05 IS 'Next action 5 name';
COMMENT ON COLUMN cv.nd06 IS 'Next action 6 date';
COMMENT ON COLUMN cv.nt06 IS 'Next action 6 time';
COMMENT ON COLUMN cv.nx06 IS 'Next action 6 name';
COMMENT ON COLUMN cv.nd07 IS 'Next action 7 date';
COMMENT ON COLUMN cv.nt07 IS 'Next action 7 time';
COMMENT ON COLUMN cv.nx07 IS 'Next action 7 name';
COMMENT ON COLUMN cv.nd08 IS 'Next action 8 date';
COMMENT ON COLUMN cv.nt08 IS 'Next action 8 time';
COMMENT ON COLUMN cv.nx08 IS 'Next action 8 name';
COMMENT ON COLUMN cv.nd09 IS 'Next action 9 date';
COMMENT ON COLUMN cv.nt09 IS 'Next action 9 time';
COMMENT ON COLUMN cv.nx09 IS 'Next action 9 name';
COMMENT ON COLUMN cv.nd10 IS 'Next action 10 date';
COMMENT ON COLUMN cv.nt10 IS 'Next action 10 time';
COMMENT ON COLUMN cv.nx10 IS 'Next action 10 name';
COMMENT ON COLUMN cv.nd11 IS 'Next action 11 date';
COMMENT ON COLUMN cv.nt11 IS 'Next action 11 time';
COMMENT ON COLUMN cv.nx11 IS 'Next action 11 name';
COMMENT ON COLUMN cv.nd12 IS 'Next action 12 date';
COMMENT ON COLUMN cv.nt12 IS 'Next action 12 time';
COMMENT ON COLUMN cv.nx12 IS 'Next action 12 name';
COMMENT ON COLUMN cv.nd13 IS 'Next action 13 date';
COMMENT ON COLUMN cv.nt13 IS 'Next action 13 time';
COMMENT ON COLUMN cv.nx13 IS 'Next action 13 name';
COMMENT ON COLUMN cv.nd14 IS 'Next action 14 date';
COMMENT ON COLUMN cv.nt14 IS 'Next action 14 time';
COMMENT ON COLUMN cv.nx14 IS 'Next action 14 name';
COMMENT ON COLUMN cv.nd15 IS 'Next action 15 date';
COMMENT ON COLUMN cv.nt15 IS 'Next action 15 time';
COMMENT ON COLUMN cv.nx15 IS 'Next action 15 name';
COMMENT ON COLUMN cv.nm01 IS 'Minutes to perform next action 1';
COMMENT ON COLUMN cv.nm02 IS 'Minutes to perform next action 2';
COMMENT ON COLUMN cv.nm03 IS 'Minutes to perform next action 3';
COMMENT ON COLUMN cv.nm04 IS 'Minutes to perform next action 4';
COMMENT ON COLUMN cv.nm05 IS 'Minutes to perform next action 5';
COMMENT ON COLUMN cv.nm06 IS 'Minutes to perform next action 6';
COMMENT ON COLUMN cv.nm07 IS 'Minutes to perform next action 7';
COMMENT ON COLUMN cv.nm08 IS 'Minutes to perform next action 8';
COMMENT ON COLUMN cv.nm09 IS 'Minutes to perform next action 9';
COMMENT ON COLUMN cv.nm10 IS 'Minutes to perform next action 10';
COMMENT ON COLUMN cv.nm11 IS 'Minutes to perform next action 11';
COMMENT ON COLUMN cv.nm12 IS 'Minutes to perform next action 12';
COMMENT ON COLUMN cv.nm13 IS 'Minutes to perform next action 13';
COMMENT ON COLUMN cv.nm14 IS 'Minutes to perform next action 14';
COMMENT ON COLUMN cv.nm15 IS 'Minutes to perform next action 15';
COMMENT ON COLUMN cv.regc IS 'Regular collection';
COMMENT ON COLUMN cv.dely IS 'Delay in delivery';
COMMENT ON COLUMN cv.cdly IS 'Delay in collection';
COMMENT ON COLUMN cv.ocol IS 'Delivery address rule';
COMMENT ON COLUMN cv.thtm IS 'Last tracking time';
COMMENT ON COLUMN cv.thdt IS 'Last tracking date';
COMMENT ON COLUMN cv.smlp IS 'Last tracking milestone';
COMMENT ON COLUMN cv.thtx IS 'Last tracking text';
COMMENT ON COLUMN cv.tpcl IS 'jobs collection/delivery address type';
COMMENT ON COLUMN cv.odel IS 'Collection address rule';
COMMENT ON COLUMN cv.sp01 IS 'Sales price analysys 1';
COMMENT ON COLUMN cv.sp02 IS 'Sales price analysys 2';
COMMENT ON COLUMN cv.sp03 IS 'Sales price analysys 3';
COMMENT ON COLUMN cv.sp04 IS 'Sales price analysys 4';
COMMENT ON COLUMN cv.sp05 IS 'Sales price analysys 5';
COMMENT ON COLUMN cv.sp06 IS 'Sales price analysys 6';
COMMENT ON COLUMN cv.sp07 IS 'Sales price analysys 7';
COMMENT ON COLUMN cv.sp08 IS 'Sales price analysys 8';
COMMENT ON COLUMN cv.sp09 IS 'Sales price analysys 9';
COMMENT ON COLUMN cv.sp10 IS 'Sales price analysys 10';
COMMENT ON COLUMN cv.sp11 IS 'Sales price analysys 11';
COMMENT ON COLUMN cv.sp12 IS 'Sales price analysys 12';
COMMENT ON COLUMN cv.sp13 IS 'Sales price analysys 13';
COMMENT ON COLUMN cv.sp14 IS 'Sales price analysys 14';
COMMENT ON COLUMN cv.sp15 IS 'Sales price analysys 15';
COMMENT ON COLUMN cv.sp16 IS 'Sales price analysys 16';
COMMENT ON COLUMN cv.sp17 IS 'Sales price analysys 17';
COMMENT ON COLUMN cv.sp18 IS 'Sales price analysys 18';
COMMENT ON COLUMN cv.sp19 IS 'Sales price analysys 19';
COMMENT ON COLUMN cv.sp20 IS 'Sales price analysys 20';
COMMENT ON COLUMN cv.cp01 IS 'Sales price analysys 1';
COMMENT ON COLUMN cv.cp02 IS 'Cost price analysys 2';
COMMENT ON COLUMN cv.cp03 IS 'Cost price analysys 3';
COMMENT ON COLUMN cv.cp04 IS 'Cost price analysys 4';
COMMENT ON COLUMN cv.cp05 IS 'Cost price analysys 5';
COMMENT ON COLUMN cv.cp06 IS 'Cost price analysys 6';
COMMENT ON COLUMN cv.cp07 IS 'Cost price analysys 7';
COMMENT ON COLUMN cv.cp08 IS 'Cost price analysys 8';
COMMENT ON COLUMN cv.cp09 IS 'Cost price analysys 9';
COMMENT ON COLUMN cv.cp10 IS 'Cost price analysys 10';
COMMENT ON COLUMN cv.cp11 IS 'Cost price analysys 11';
COMMENT ON COLUMN cv.cp12 IS 'Cost price analysys 12';
COMMENT ON COLUMN cv.cp13 IS 'Cost price analysys 13';
COMMENT ON COLUMN cv.cp14 IS 'Cost price analysys 14';
COMMENT ON COLUMN cv.cp15 IS 'Cost price analysys 15';
COMMENT ON COLUMN cv.cp16 IS 'Cost price analysys 16';
COMMENT ON COLUMN cv.cp17 IS 'Cost price analysys 17';
COMMENT ON COLUMN cv.cp18 IS 'Cost price analysys 18';
COMMENT ON COLUMN cv.cp19 IS 'Cost price analysys 19';
COMMENT ON COLUMN cv.cp20 IS 'Cost price analysys 20';
CREATE SEQUENCE cy_bookmark_seq
    START WITH 1224979098644775168
    INCREMENT BY 1
    MINVALUE 1224979098644775168
    MAXVALUE 1261007895663738879
    CACHE 1;
ALTER TABLE cy_bookmark_seq OWNER TO ncuser;
CREATE TABLE cy (
    _bookmark_ bigint DEFAULT nextval('cy_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    cuid character varying,
    sdat date,
    edat date,
    rate numeric(12,6),
    stat character varying(1),
    vsno integer,
    cyid character varying
);
ALTER TABLE cy OWNER TO ncuser;
COMMENT ON TABLE cy IS 'Currency Rate File';
COMMENT ON COLUMN cy.crcd IS 'Courier Code';
COMMENT ON COLUMN cy.cuid IS 'Currency Id';
COMMENT ON COLUMN cy.sdat IS 'Start Date';
COMMENT ON COLUMN cy.edat IS 'End Date';
COMMENT ON COLUMN cy.rate IS 'Value Rate';
COMMENT ON COLUMN cy.stat IS 'Record Status';
COMMENT ON COLUMN cy.vsno IS 'Version No.';
COMMENT ON COLUMN cy.cyid IS 'Currency rate id';
CREATE SEQUENCE da_bookmark_seq
    START WITH 2341871806232658176
    INCREMENT BY 1
    MINVALUE 2341871806232658176
    MAXVALUE 2377900603251621887
    CACHE 1;
ALTER TABLE da_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE dc_bookmark_seq
    START WITH 3927138875067072768
    INCREMENT BY 1
    MINVALUE 3927138875067072768
    MAXVALUE 3963167672086036479
    CACHE 1;
ALTER TABLE dc_bookmark_seq OWNER TO ncuser;
CREATE TABLE dc (
    _bookmark_ bigint DEFAULT nextval('dc_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    dccd character varying,
    dcid character varying,
    seqn character varying,
    orjt character varying,
    stat character varying(1),
    crus character varying,
    crdt date,
    crtm character varying,
    dlus character varying,
    dldt date,
    dltm character varying,
    crre character varying,
    "DESC" character varying,
    lurl character varying,
    type character varying,
    lock character varying(1),
    objt character varying,
    fnam character varying,
    ealp character varying(1),
    ispd character varying(1),
    vsno integer,
    docs numeric(12,3)
);
ALTER TABLE dc OWNER TO ncuser;
COMMENT ON TABLE dc IS 'Document file';
COMMENT ON COLUMN dc.crcd IS 'Courier Code';
COMMENT ON COLUMN dc.dccd IS 'object value';
COMMENT ON COLUMN dc.dcid IS 'Document id';
COMMENT ON COLUMN dc.seqn IS 'Sequence Number';
COMMENT ON COLUMN dc.orjt IS 'Original transaction id';
COMMENT ON COLUMN dc.stat IS 'Status';
COMMENT ON COLUMN dc.crus IS 'Created user id';
COMMENT ON COLUMN dc.crdt IS 'Created date';
COMMENT ON COLUMN dc.crtm IS 'Created time';
COMMENT ON COLUMN dc.dlus IS 'Deleted user id';
COMMENT ON COLUMN dc.dldt IS 'Deleted date';
COMMENT ON COLUMN dc.dltm IS 'Deleted time';
COMMENT ON COLUMN dc.crre IS 'Created recipient';
COMMENT ON COLUMN dc."DESC" IS 'Description';
COMMENT ON COLUMN dc.lurl IS 'URL';
COMMENT ON COLUMN dc.type IS 'Type(Document extension)';
COMMENT ON COLUMN dc.lock IS 'Locked';
COMMENT ON COLUMN dc.objt IS 'Object name (CS CL etc)';
COMMENT ON COLUMN dc.fnam IS 'URL';
COMMENT ON COLUMN dc.ealp IS 'Enable auto label print';
COMMENT ON COLUMN dc.ispd IS 'Is private document';
COMMENT ON COLUMN dc.vsno IS 'Version no.';
COMMENT ON COLUMN dc.docs IS 'Document size';
CREATE SEQUENCE de_bookmark_seq
    START WITH 2846274964498153728
    INCREMENT BY 1
    MINVALUE 2846274964498153728
    MAXVALUE 2882303761517117439
    CACHE 1;
ALTER TABLE de_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE di_bookmark_seq
    START WITH 4143311657180856576
    INCREMENT BY 1
    MINVALUE 4143311657180856576
    MAXVALUE 4179340454199820287
    CACHE 1;
ALTER TABLE di_bookmark_seq OWNER TO ncuser;
CREATE TABLE di (
    _bookmark_ bigint DEFAULT nextval('di_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    diid character varying,
    prgm character varying,
    levl character varying,
    usid character varying,
    date date,
    "time" character varying,
    "DESC" character varying,
    ijob character varying,
    stat character varying,
    vsno integer
);
ALTER TABLE di OWNER TO ncuser;
COMMENT ON TABLE di IS 'Debug info';
COMMENT ON COLUMN di.crcd IS 'Courier Code';
COMMENT ON COLUMN di.diid IS 'Debug id';
COMMENT ON COLUMN di.prgm IS 'Program name';
COMMENT ON COLUMN di.levl IS 'Debug level';
COMMENT ON COLUMN di.usid IS 'User code';
COMMENT ON COLUMN di.date IS 'Date';
COMMENT ON COLUMN di."time" IS 'Time';
COMMENT ON COLUMN di."DESC" IS 'Description';
COMMENT ON COLUMN di.ijob IS 'Process id';
CREATE SEQUENCE do_bookmark_seq
    START WITH 1333065489701667072
    INCREMENT BY 1
    MINVALUE 1333065489701667072
    MAXVALUE 1369094286720630783
    CACHE 1;
ALTER TABLE do_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE dr_bookmark_seq
    START WITH 1369094286720631040
    INCREMENT BY 1
    MINVALUE 1369094286720631040
    MAXVALUE 1405123083739594751
    CACHE 1;
ALTER TABLE dr_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE ds_bookmark_seq
    START WITH 3350678122763649280
    INCREMENT BY 1
    MINVALUE 3350678122763649280
    MAXVALUE 3386706919782612991
    CACHE 1;
ALTER TABLE ds_bookmark_seq OWNER TO ncuser;
CREATE TABLE ds (
    _bookmark_ bigint DEFAULT nextval('ds_bookmark_seq'::regclass) NOT NULL,
    date date,
    epax integer,
    eval numeric(11,2),
    emgn numeric(11,2),
    opax integer,
    oval numeric(11,2),
    omgn numeric(11,2),
    spax integer,
    sval numeric(11,2),
    smgn numeric(11,2),
    bpax integer,
    bval numeric(11,2),
    bmgn numeric(11,2),
    cpax integer,
    cval numeric(11,2),
    cmgn numeric(11,2),
    ipax integer,
    ival numeric(11,2),
    imgn numeric(11,2),
    enum integer,
    onum integer,
    snum integer,
    bnum integer,
    cnum integer,
    inum integer,
    sucd character varying
);
ALTER TABLE ds OWNER TO ncuser;
COMMENT ON TABLE ds IS 'Daily Sales';
COMMENT ON COLUMN ds.date IS 'Date';
COMMENT ON COLUMN ds.epax IS 'Enquiries - passengers';
COMMENT ON COLUMN ds.eval IS 'Enquiries - net value';
COMMENT ON COLUMN ds.emgn IS 'Enquiries - margin';
COMMENT ON COLUMN ds.opax IS 'Options - passengers';
COMMENT ON COLUMN ds.oval IS 'Options - net value';
COMMENT ON COLUMN ds.omgn IS 'Options - margin';
COMMENT ON COLUMN ds.spax IS 'Sales - passengers';
COMMENT ON COLUMN ds.sval IS 'Sales - net value';
COMMENT ON COLUMN ds.smgn IS 'Sales - margin';
COMMENT ON COLUMN ds.bpax IS 'Bookings - passengers';
COMMENT ON COLUMN ds.bval IS 'Bookings - net value';
COMMENT ON COLUMN ds.bmgn IS 'Bookings - margin';
COMMENT ON COLUMN ds.cpax IS 'Cancellations - passengers';
COMMENT ON COLUMN ds.cval IS 'Cancellations - net value';
COMMENT ON COLUMN ds.cmgn IS 'Cancellations - margin';
COMMENT ON COLUMN ds.ipax IS 'Invoiced - passengers';
COMMENT ON COLUMN ds.ival IS 'Invoiced - net value';
COMMENT ON COLUMN ds.imgn IS 'Invoiced - margin';
COMMENT ON COLUMN ds.enum IS 'Enquiries - bookings';
COMMENT ON COLUMN ds.onum IS 'Options - bookings';
COMMENT ON COLUMN ds.snum IS 'Sales - bookings';
COMMENT ON COLUMN ds.bnum IS 'Bookings - bookings';
COMMENT ON COLUMN ds.cnum IS 'Cancellations - bookings';
COMMENT ON COLUMN ds.inum IS 'Invoices - bookings';
COMMENT ON COLUMN ds.sucd IS 'Supplier for Daily Sales';
CREATE SEQUENCE dt_bookmark_seq
    START WITH 3746994889972252928
    INCREMENT BY 1
    MINVALUE 3746994889972252928
    MAXVALUE 3783023686991216639
    CACHE 1;
ALTER TABLE dt_bookmark_seq OWNER TO ncuser;
CREATE TABLE dt (
    _bookmark_ bigint DEFAULT nextval('dt_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    dtcd character varying,
    dtid character varying,
    "DESC" character varying,
    rtid character varying,
    tday integer,
    atim character varying,
    ctim character varying,
    dtim character varying,
    stat character varying(1),
    vsno integer
);
ALTER TABLE dt OWNER TO ncuser;
COMMENT ON TABLE dt IS 'Delivery times';
COMMENT ON COLUMN dt.crcd IS 'Courier Code';
COMMENT ON COLUMN dt.dtcd IS 'Short code';
COMMENT ON COLUMN dt.dtid IS 'Unique Id';
COMMENT ON COLUMN dt."DESC" IS 'Name of day';
COMMENT ON COLUMN dt.rtid IS 'Route id';
COMMENT ON COLUMN dt.tday IS 'Transit days';
COMMENT ON COLUMN dt.atim IS 'Arrival time';
COMMENT ON COLUMN dt.ctim IS 'Cut off time';
COMMENT ON COLUMN dt.dtim IS 'Departure time';
COMMENT ON COLUMN dt.stat IS 'Record Status';
CREATE SEQUENCE ea_bookmark_seq
    START WITH 3674937295934324992
    INCREMENT BY 1
    MINVALUE 3674937295934324992
    MAXVALUE 3710966092953288703
    CACHE 1;
ALTER TABLE ea_bookmark_seq OWNER TO ncuser;
CREATE TABLE ea (
    _bookmark_ bigint DEFAULT nextval('ea_bookmark_seq'::regclass) NOT NULL,
    eaid character varying,
    crcd character varying,
    exid character varying,
    fzid character varying,
    tzid character varying,
    refr character varying,
    fbnd numeric(12,4),
    tbnd numeric(12,4),
    eval numeric(11,3),
    prio character varying,
    bass character varying,
    unit character varying,
    stat character varying(1),
    vsno integer,
    cuid character varying,
    plid character varying,
    chml character varying,
    buml character varying,
    chpc character varying(1),
    dvfr date
);
ALTER TABLE ea OWNER TO ncuser;
COMMENT ON TABLE ea IS 'Extra Allocation';
COMMENT ON COLUMN ea.eaid IS 'Extra allocation ID';
COMMENT ON COLUMN ea.crcd IS 'Courier Code';
COMMENT ON COLUMN ea.exid IS 'Extra ID';
COMMENT ON COLUMN ea.fzid IS 'Zone from ID';
COMMENT ON COLUMN ea.tzid IS 'Zone to ID';
COMMENT ON COLUMN ea.refr IS 'Client or Supplier ID';
COMMENT ON COLUMN ea.fbnd IS 'From Band';
COMMENT ON COLUMN ea.tbnd IS 'To Band';
COMMENT ON COLUMN ea.eval IS 'Extra value';
COMMENT ON COLUMN ea.prio IS 'Priority';
COMMENT ON COLUMN ea.bass IS 'Basis';
COMMENT ON COLUMN ea.unit IS 'Basis';
COMMENT ON COLUMN ea.stat IS 'Status';
COMMENT ON COLUMN ea.vsno IS 'Version no';
COMMENT ON COLUMN ea.cuid IS 'Currency id';
COMMENT ON COLUMN ea.plid IS 'Place id';
COMMENT ON COLUMN ea.chml IS 'Charging multiplier';
COMMENT ON COLUMN ea.buml IS 'Band unit multiplier';
COMMENT ON COLUMN ea.chpc IS 'Percentage of unit cost';
COMMENT ON COLUMN ea.dvfr IS 'Valid from date';
CREATE SEQUENCE en_bookmark_seq
    START WITH 2413929400270586112
    INCREMENT BY 1
    MINVALUE 2413929400270586112
    MAXVALUE 2449958197289549823
    CACHE 1;
ALTER TABLE en_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE ex_bookmark_seq
    START WITH 3638908498915361024
    INCREMENT BY 1
    MINVALUE 3638908498915361024
    MAXVALUE 3674937295934324735
    CACHE 1;
ALTER TABLE ex_bookmark_seq OWNER TO ncuser;
CREATE TABLE ex (
    _bookmark_ bigint DEFAULT nextval('ex_bookmark_seq'::regclass) NOT NULL,
    exid character varying,
    crcd character varying,
    excd character varying,
    type character varying(1),
    itxt character varying,
    pdid character varying,
    meth character varying,
    stax character varying(1),
    prio character varying,
    vsno integer,
    stat character varying(1),
    ctyp character varying,
    unit character varying,
    tdes character varying,
    mdes character varying,
    txcd character varying,
    plac character varying,
    slid character varying
);
ALTER TABLE ex OWNER TO ncuser;
COMMENT ON TABLE ex IS 'Extra';
COMMENT ON COLUMN ex.exid IS 'Extra ID';
COMMENT ON COLUMN ex.crcd IS 'Courier Code';
COMMENT ON COLUMN ex.excd IS 'Extra code';
COMMENT ON COLUMN ex.type IS 'Sales or Cost flag';
COMMENT ON COLUMN ex.itxt IS 'Invoice text';
COMMENT ON COLUMN ex.pdid IS 'Product ID (blank for all)';
COMMENT ON COLUMN ex.meth IS 'Method of Extras';
COMMENT ON COLUMN ex.stax IS 'Sales Tax (vat) applies';
COMMENT ON COLUMN ex.prio IS 'Priority of extra app. on booking';
COMMENT ON COLUMN ex.vsno IS 'Version No';
COMMENT ON COLUMN ex.stat IS 'Status';
COMMENT ON COLUMN ex.ctyp IS 'Contents Id';
COMMENT ON COLUMN ex.unit IS 'Pricing unit';
COMMENT ON COLUMN ex.tdes IS 'Type description';
COMMENT ON COLUMN ex.mdes IS 'Method description';
COMMENT ON COLUMN ex.txcd IS 'Tax rate code';
COMMENT ON COLUMN ex.plac IS 'Price line analysis code';
COMMENT ON COLUMN ex.slid IS 'Service level id';
CREATE SEQUENCE fi_bookmark_seq
    START WITH 2017612633061982464
    INCREMENT BY 1
    MINVALUE 2017612633061982464
    MAXVALUE 2053641430080946175
    CACHE 1;
ALTER TABLE fi_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE fields_bookmark_seq
    START WITH 180143985094820096
    INCREMENT BY 1
    MINVALUE 180143985094820096
    MAXVALUE 216172782113783807
    CACHE 1;
ALTER TABLE fields_bookmark_seq OWNER TO ncuser;
CREATE TABLE fields (
    _bookmark_ bigint DEFAULT nextval('fields_bookmark_seq'::regclass) NOT NULL,
    name character varying,
    sname character varying,
    title text,
    datatype character varying,
    compl character varying,
    disp character varying,
    step character varying,
    fnum integer,
    help character varying,
    num integer,
    dnum integer,
    rhd1 character varying,
    rhd2 character varying
);
ALTER TABLE fields OWNER TO ncuser;
COMMENT ON TABLE fields IS 'System File 5 - Fields';
COMMENT ON COLUMN fields.name IS '[5.1] Logical field name';
COMMENT ON COLUMN fields.sname IS '[5.2] Global field name';
COMMENT ON COLUMN fields.title IS '[5.3] Description';
COMMENT ON COLUMN fields.datatype IS '[5.4] Validation type';
COMMENT ON COLUMN fields.compl IS '[5.5] Compulsory';
COMMENT ON COLUMN fields.disp IS '[5.6] Display';
COMMENT ON COLUMN fields.step IS '[5.8] Step';
COMMENT ON COLUMN fields.fnum IS '[5.9] Logical file no.';
COMMENT ON COLUMN fields.help IS '[5.10] Help screen name';
COMMENT ON COLUMN fields.num IS '[5.11] Field no.';
COMMENT ON COLUMN fields.dnum IS '[5.12] Display no.';
COMMENT ON COLUMN fields.rhd1 IS '[5.13] Report heading 1';
COMMENT ON COLUMN fields.rhd2 IS '[5.14] Report heading 2';
CREATE SEQUENCE files_bookmark_seq
    START WITH 108086391056892160
    INCREMENT BY 1
    MINVALUE 108086391056892160
    MAXVALUE 144115188075855871
    CACHE 1;
ALTER TABLE files_bookmark_seq OWNER TO ncuser;
CREATE TABLE files (
    _bookmark_ bigint DEFAULT nextval('files_bookmark_seq'::regclass) NOT NULL,
    name character varying,
    num integer,
    title text,
    pname character varying,
    lfldnu integer,
    ldispn integer
);
ALTER TABLE files OWNER TO ncuser;
COMMENT ON TABLE files IS 'System File 3 - Logical files';
COMMENT ON COLUMN files.name IS '[3.1] Logical file name';
COMMENT ON COLUMN files.num IS '[3.2] Logical file no.';
COMMENT ON COLUMN files.title IS '[3.3] Description';
COMMENT ON COLUMN files.pname IS '[3.4] Physical file name';
COMMENT ON COLUMN files.lfldnu IS '[3.5] Last field no. allocated';
COMMENT ON COLUMN files.ldispn IS '[3.6] Last display no. allocated';
CREATE SEQUENCE ft_bookmark_seq
    START WITH 1405123083739595008
    INCREMENT BY 1
    MINVALUE 1405123083739595008
    MAXVALUE 1441151880758558719
    CACHE 1;
ALTER TABLE ft_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE ga_bookmark_seq
    START WITH 1441151880758558976
    INCREMENT BY 1
    MINVALUE 1441151880758558976
    MAXVALUE 1477180677777522687
    CACHE 1;
ALTER TABLE ga_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE gflds_bookmark_seq
    START WITH 144115188075855872
    INCREMENT BY 1
    MINVALUE 144115188075855872
    MAXVALUE 180143985094819839
    CACHE 1;
ALTER TABLE gflds_bookmark_seq OWNER TO ncuser;
CREATE TABLE gflds (
    _bookmark_ bigint DEFAULT nextval('gflds_bookmark_seq'::regclass) NOT NULL,
    name character varying,
    title character varying,
    datatype character varying,
    compul character varying,
    disp character varying,
    help character varying
);
ALTER TABLE gflds OWNER TO ncuser;
COMMENT ON TABLE gflds IS 'System File 4 - Global fields';
COMMENT ON COLUMN gflds.name IS '[4.1] GLobal field name';
COMMENT ON COLUMN gflds.title IS '[4.2] Description';
COMMENT ON COLUMN gflds.datatype IS '[4.3] Validation type';
COMMENT ON COLUMN gflds.compul IS '[4.4] Compulsory';
COMMENT ON COLUMN gflds.disp IS '[4.5] Display';
COMMENT ON COLUMN gflds.help IS '[4.6] Help screen';
CREATE SEQUENCE global_id_sequence
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE global_id_sequence OWNER TO ncuser;
CREATE SEQUENCE gt_bookmark_seq
    START WITH 3819052484010180864
    INCREMENT BY 1
    MINVALUE 3819052484010180864
    MAXVALUE 3855081281029144575
    CACHE 1;
ALTER TABLE gt_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE helps_bookmark_seq
    START WITH 360287970189639680
    INCREMENT BY 1
    MINVALUE 360287970189639680
    MAXVALUE 396316767208603647
    CACHE 1;
ALTER TABLE helps_bookmark_seq OWNER TO ncuser;
CREATE TABLE helps (
    _bookmark_ bigint DEFAULT nextval('helps_bookmark_seq'::regclass) NOT NULL,
    name character varying,
    mesg character varying,
    line1 character varying,
    line2 character varying,
    line3 character varying,
    line4 character varying,
    line5 character varying,
    line6 character varying,
    line7 character varying,
    line8 character varying,
    line9 character varying,
    line10 character varying,
    line11 character varying,
    line12 character varying,
    line13 character varying,
    line14 character varying,
    line15 character varying,
    line16 character varying,
    line17 character varying,
    line18 character varying,
    line19 character varying,
    line20 character varying,
    dlines integer,
    dcols integer
);
ALTER TABLE helps OWNER TO ncuser;
COMMENT ON TABLE helps IS 'System File 10 - Help screens';
COMMENT ON COLUMN helps.name IS '[10.1] Help screen name';
COMMENT ON COLUMN helps.mesg IS '[10.2] Brief help message';
COMMENT ON COLUMN helps.line1 IS '[10.22] Full help line 20';
COMMENT ON COLUMN helps.dlines IS '[10.30] Screen dimension (lines)';
COMMENT ON COLUMN helps.dcols IS '[10.31] Screen dimension (columns)';
CREATE SEQUENCE hn_bookmark_seq
    START WITH 3170534137668829440
    INCREMENT BY 1
    MINVALUE 3170534137668829440
    MAXVALUE 3206562934687793151
    CACHE 1;
ALTER TABLE hn_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE ho_bookmark_seq
    START WITH 3422735716801577216
    INCREMENT BY 1
    MINVALUE 3422735716801577216
    MAXVALUE 3458764513820540927
    CACHE 1;
ALTER TABLE ho_bookmark_seq OWNER TO ncuser;
CREATE TABLE ho (
    _bookmark_ bigint DEFAULT nextval('ho_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    coid character varying,
    fdat date,
    edat date,
    "DESC" character varying,
    stat character varying(1),
    vsno integer,
    hoid character varying
);
ALTER TABLE ho OWNER TO ncuser;
COMMENT ON TABLE ho IS 'Holiday File';
COMMENT ON COLUMN ho.crcd IS 'Courier Code';
COMMENT ON COLUMN ho.coid IS 'Country Id';
COMMENT ON COLUMN ho.fdat IS 'From Date';
COMMENT ON COLUMN ho.edat IS 'End Date';
COMMENT ON COLUMN ho."DESC" IS 'Description';
COMMENT ON COLUMN ho.stat IS 'Record Status';
COMMENT ON COLUMN ho.vsno IS 'version No.';
COMMENT ON COLUMN ho.hoid IS 'Unique id';
CREATE SEQUENCE ip_bookmark_seq
    START WITH 1369094286720631040
    INCREMENT BY 1
    MINVALUE 1369094286720631040
    MAXVALUE 1405123083739594751
    CACHE 1;
ALTER TABLE ip_bookmark_seq OWNER TO ncuser;
CREATE TABLE ip (
    _bookmark_ bigint DEFAULT nextval('ip_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    ipid character varying,
    crus character varying,
    crdt date,
    crtm character varying,
    stat character varying(1),
    pmid character varying,
    siid character varying,
    amnt numeric(10,2),
    amn2 numeric(10,2),
    clid character varying
);
ALTER TABLE ip OWNER TO ncuser;
COMMENT ON TABLE ip IS 'Payment';
COMMENT ON COLUMN ip.crcd IS 'Courier Code';
COMMENT ON COLUMN ip.ipid IS 'Unique Id';
COMMENT ON COLUMN ip.crus IS 'User id';
COMMENT ON COLUMN ip.crdt IS 'Date of data entry';
COMMENT ON COLUMN ip.crtm IS 'Time of data entry';
COMMENT ON COLUMN ip.stat IS 'Status of the record';
COMMENT ON COLUMN ip.pmid IS 'Payment Id';
COMMENT ON COLUMN ip.siid IS 'Invoice Id';
COMMENT ON COLUMN ip.amnt IS 'Allocation amount in courier currency';
COMMENT ON COLUMN ip.amn2 IS 'Amount in client currency';
COMMENT ON COLUMN ip.clid IS 'Client Id';
CREATE SEQUENCE is_bookmark_seq
    START WITH 3674937295934324992
    INCREMENT BY 1
    MINVALUE 3674937295934324992
    MAXVALUE 3710966092953288703
    CACHE 1;
ALTER TABLE is_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE it_bookmark_seq
    START WITH 900719925474099456
    INCREMENT BY 1
    MINVALUE 900719925474099456
    MAXVALUE 936748722493063167
    CACHE 1;
ALTER TABLE it_bookmark_seq OWNER TO ncuser;
CREATE TABLE it (
    _bookmark_ bigint DEFAULT nextval('it_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    itid character varying,
    itcd character varying,
    "DESC" character varying,
    stat character varying(1),
    vsno integer,
    clid character varying,
    tags character varying,
    cref character varying,
    utx1 character varying,
    utx2 character varying,
    ppqt character varying,
    tqty character varying,
    smin character varying,
    loid character varying,
    iloc character varying,
    wgth numeric(6,2),
    mlvl integer,
    exdt date,
    lbid date,
    lbod date,
    high numeric(8,2),
    lnth numeric(8,2),
    widt numeric(8,2)
);
ALTER TABLE it OWNER TO ncuser;
COMMENT ON TABLE it IS 'Item';
COMMENT ON COLUMN it.crcd IS 'Courier Code';
COMMENT ON COLUMN it.itid IS 'Item id';
COMMENT ON COLUMN it.itcd IS 'Item code';
COMMENT ON COLUMN it."DESC" IS 'Description';
COMMENT ON COLUMN it.stat IS 'Status';
COMMENT ON COLUMN it.vsno IS 'Version No';
COMMENT ON COLUMN it.clid IS 'Client id';
COMMENT ON COLUMN it.tags IS 'Tags';
COMMENT ON COLUMN it.cref IS 'Customer reference';
COMMENT ON COLUMN it.utx1 IS 'User defined data 1';
COMMENT ON COLUMN it.utx2 IS 'User defined data 2';
COMMENT ON COLUMN it.ppqt IS 'Per pack quantity';
COMMENT ON COLUMN it.tqty IS 'Current no in stock';
COMMENT ON COLUMN it.smin IS 'Minimum stock level';
COMMENT ON COLUMN it.loid IS 'Location id';
COMMENT ON COLUMN it.iloc IS 'Relative path of the stock image';
COMMENT ON COLUMN it.wgth IS 'Weight per pack';
COMMENT ON COLUMN it.mlvl IS 'Minimum stock level- depricated';
COMMENT ON COLUMN it.exdt IS 'Expiry date';
COMMENT ON COLUMN it.lbid IS 'Last book-in date';
COMMENT ON COLUMN it.lbod IS 'Last book-out date';
COMMENT ON COLUMN it.high IS 'Height of the item';
COMMENT ON COLUMN it.lnth IS 'Length of the item';
COMMENT ON COLUMN it.widt IS 'Width of the item';
CREATE SEQUENCE ja_bookmark_seq
    START WITH 3963167672086036736
    INCREMENT BY 1
    MINVALUE 3963167672086036736
    MAXVALUE 3999196469105000447
    CACHE 1;
ALTER TABLE ja_bookmark_seq OWNER TO ncuser;
CREATE TABLE ja (
    _bookmark_ bigint DEFAULT nextval('ja_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    csid character varying,
    jaid character varying,
    seqn character varying,
    orjt character varying,
    stat character varying(1),
    crus character varying,
    crdt date,
    crtm character varying,
    dlus character varying,
    dldt date,
    dltm character varying,
    ntbl character varying,
    noid character varying,
    note character varying,
    disp character varying,
    trnt character varying(1),
    lock character varying(1)
);
ALTER TABLE ja OWNER TO ncuser;
COMMENT ON TABLE ja IS 'Job alert file';
COMMENT ON COLUMN ja.crcd IS 'Courier Code';
COMMENT ON COLUMN ja.csid IS 'Job id';
COMMENT ON COLUMN ja.jaid IS 'Job alert id';
COMMENT ON COLUMN ja.seqn IS 'Sequence Number';
COMMENT ON COLUMN ja.orjt IS 'Original transaction id';
COMMENT ON COLUMN ja.stat IS 'Status';
COMMENT ON COLUMN ja.crus IS 'Created user id';
COMMENT ON COLUMN ja.crdt IS 'Created date';
COMMENT ON COLUMN ja.crtm IS 'Created time';
COMMENT ON COLUMN ja.dlus IS 'Deleted user id';
COMMENT ON COLUMN ja.dldt IS 'Deleted date';
COMMENT ON COLUMN ja.dltm IS 'Deleted time';
COMMENT ON COLUMN ja.ntbl IS 'Note table';
COMMENT ON COLUMN ja.noid IS 'Note id';
COMMENT ON COLUMN ja.note IS 'Note text';
COMMENT ON COLUMN ja.disp IS 'When to display';
COMMENT ON COLUMN ja.trnt IS 'Add Tracking Note';
COMMENT ON COLUMN ja.lock IS 'Locked';
CREATE SEQUENCE jl_bookmark_seq
    START WITH 3999196469105000704
    INCREMENT BY 1
    MINVALUE 3999196469105000704
    MAXVALUE 4035225266123964415
    CACHE 1;
ALTER TABLE jl_bookmark_seq OWNER TO ncuser;
CREATE TABLE jl (
    _bookmark_ bigint DEFAULT nextval('jl_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    csid character varying,
    jlid character varying,
    seqn character varying,
    orjt character varying,
    stat character varying(1),
    crus character varying,
    crdt date,
    crtm character varying,
    dlus character varying,
    dldt date,
    dltm character varying,
    lgid character varying,
    mfid character varying,
    rtid character varying,
    suid character varying,
    bagn character varying,
    lock character varying(1),
    styp character varying,
    fpid character varying,
    tpid character varying,
    trck character varying(1),
    lgcd character varying,
    pcid character varying,
    wfid character varying,
    objt character varying,
    type character varying,
    fzid character varying,
    tzid character varying,
    srf2 character varying,
    serv character varying,
    rout character varying,
    dvdt date,
    lblu character varying,
    pbdt date,
    pbtm character varying,
    cndt date,
    cntm character varying,
    bvia character varying,
    cuto character varying,
    dtim character varying,
    atim character varying,
    ldes character varying,
    aubn integer,
    ud01 character varying,
    ud02 character varying,
    ud03 character varying,
    ud04 character varying,
    ud05 character varying,
    ud06 character varying,
    ud07 character varying,
    ud08 character varying,
    ud09 character varying,
    ud10 character varying,
    ud11 character varying,
    ud12 character varying,
    ud13 character varying,
    ud14 character varying,
    ud15 character varying,
    ud16 character varying,
    ud17 character varying,
    ud18 character varying,
    ud19 character varying,
    ud20 character varying,
    ud21 character varying,
    ud22 character varying,
    ud23 character varying,
    ud24 character varying,
    ud25 character varying,
    ud26 character varying,
    ud27 character varying,
    ud28 character varying,
    ud29 character varying,
    ud30 character varying,
    ud31 character varying,
    ud32 character varying,
    ud33 character varying,
    ud34 character varying,
    ud35 character varying,
    ud36 character varying,
    ocol character varying,
    odel character varying,
    rseq integer,
    dseq integer,
    etim character varying,
    vsno integer,
    purq character varying(1),
    albp character varying,
    albn character varying
);
ALTER TABLE jl OWNER TO ncuser;
COMMENT ON TABLE jl IS 'Job leg file';
COMMENT ON COLUMN jl.crcd IS 'Courier Code';
COMMENT ON COLUMN jl.csid IS 'Job id';
COMMENT ON COLUMN jl.jlid IS 'Job leg id';
COMMENT ON COLUMN jl.seqn IS 'Sequence Number';
COMMENT ON COLUMN jl.orjt IS 'Original transaction id';
COMMENT ON COLUMN jl.stat IS 'Status';
COMMENT ON COLUMN jl.crus IS 'Created user id';
COMMENT ON COLUMN jl.crdt IS 'Created date';
COMMENT ON COLUMN jl.crtm IS 'Created time';
COMMENT ON COLUMN jl.dlus IS 'Deleted user id';
COMMENT ON COLUMN jl.dldt IS 'Deleted date';
COMMENT ON COLUMN jl.dltm IS 'Deleted time';
COMMENT ON COLUMN jl.lgid IS 'Leg id';
COMMENT ON COLUMN jl.mfid IS 'Manifest Header ID';
COMMENT ON COLUMN jl.rtid IS 'Link to Route ID';
COMMENT ON COLUMN jl.suid IS 'Supplier Id';
COMMENT ON COLUMN jl.bagn IS 'Bag Number/ supplier ref';
COMMENT ON COLUMN jl.lock IS 'Locked';
COMMENT ON COLUMN jl.styp IS 'Source type';
COMMENT ON COLUMN jl.fpid IS 'From place';
COMMENT ON COLUMN jl.tpid IS 'To place';
COMMENT ON COLUMN jl.trck IS 'Track flag';
COMMENT ON COLUMN jl.lgcd IS 'Legcode';
COMMENT ON COLUMN jl.pcid IS 'Price chart id';
COMMENT ON COLUMN jl.wfid IS 'Workflow id';
COMMENT ON COLUMN jl.objt IS 'Object name';
COMMENT ON COLUMN jl.type IS 'Type of leg';
COMMENT ON COLUMN jl.fzid IS 'From zone';
COMMENT ON COLUMN jl.tzid IS 'To zone';
COMMENT ON COLUMN jl.srf2 IS 'Supplier 2nd';
COMMENT ON COLUMN jl.serv IS 'Supplier service';
COMMENT ON COLUMN jl.rout IS 'Supplier route';
COMMENT ON COLUMN jl.dvdt IS 'Expected delivery day';
COMMENT ON COLUMN jl.lblu IS 'Label URL';
COMMENT ON COLUMN jl.pbdt IS 'Plugins booking date';
COMMENT ON COLUMN jl.pbtm IS 'Plugins booking time';
COMMENT ON COLUMN jl.cndt IS 'Booking cancellation date';
COMMENT ON COLUMN jl.cntm IS 'Booking cancellation time';
COMMENT ON COLUMN jl.bvia IS 'Book via';
COMMENT ON COLUMN jl.cuto IS 'Cut off time';
COMMENT ON COLUMN jl.dtim IS 'Departure time';
COMMENT ON COLUMN jl.atim IS 'Arrival time';
COMMENT ON COLUMN jl.ldes IS 'Leg description';
COMMENT ON COLUMN jl.aubn IS 'Auto bag number';
COMMENT ON COLUMN jl.ud01 IS 'User defined field 1';
COMMENT ON COLUMN jl.ud02 IS 'User defined field 2';
COMMENT ON COLUMN jl.ud03 IS 'User defined field 3';
COMMENT ON COLUMN jl.ud04 IS 'User defined field 4';
COMMENT ON COLUMN jl.ud05 IS 'User defined field 5';
COMMENT ON COLUMN jl.ud06 IS 'User defined field 6';
COMMENT ON COLUMN jl.ud07 IS 'User defined field 7';
COMMENT ON COLUMN jl.ud08 IS 'User defined field 8';
COMMENT ON COLUMN jl.ud09 IS 'User defined field 9';
COMMENT ON COLUMN jl.ud10 IS 'User defined field 10';
COMMENT ON COLUMN jl.ud11 IS 'User defined field 11';
COMMENT ON COLUMN jl.ud12 IS 'User defined field 12';
COMMENT ON COLUMN jl.ud13 IS 'User defined field 13';
COMMENT ON COLUMN jl.ud14 IS 'User defined field 14';
COMMENT ON COLUMN jl.ud15 IS 'User defined field 15';
COMMENT ON COLUMN jl.ud16 IS 'User defined field 16';
COMMENT ON COLUMN jl.ud17 IS 'User defined field 17';
COMMENT ON COLUMN jl.ud18 IS 'User defined field 18';
COMMENT ON COLUMN jl.ud19 IS 'User defined field 19';
COMMENT ON COLUMN jl.ud20 IS 'User defined field 20';
COMMENT ON COLUMN jl.ud21 IS 'User defined field 21';
COMMENT ON COLUMN jl.ud22 IS 'User defined field 22';
COMMENT ON COLUMN jl.ud23 IS 'User defined field 23';
COMMENT ON COLUMN jl.ud24 IS 'User defined field 24';
COMMENT ON COLUMN jl.ud25 IS 'User defined field 25';
COMMENT ON COLUMN jl.ud26 IS 'User defined field 26';
COMMENT ON COLUMN jl.ud27 IS 'User defined field 27';
COMMENT ON COLUMN jl.ud28 IS 'User defined field 28';
COMMENT ON COLUMN jl.ud29 IS 'User defined field 29';
COMMENT ON COLUMN jl.ud30 IS 'User defined field 30';
COMMENT ON COLUMN jl.ud31 IS 'User defined field 31';
COMMENT ON COLUMN jl.ud32 IS 'User defined field 32';
COMMENT ON COLUMN jl.ud33 IS 'User defined field 33';
COMMENT ON COLUMN jl.ud34 IS 'User defined field 34';
COMMENT ON COLUMN jl.ud35 IS 'User defined field 35';
COMMENT ON COLUMN jl.ud36 IS 'User defined field 36';
COMMENT ON COLUMN jl.ocol IS 'Delivery option for collection';
COMMENT ON COLUMN jl.odel IS 'Collection option for delivery';
COMMENT ON COLUMN jl.dseq IS 'Drop sequence';
COMMENT ON COLUMN jl.etim IS 'Estimated drop time';
COMMENT ON COLUMN jl.vsno IS 'Version No.';
COMMENT ON COLUMN jl.purq IS 'Third party pickup required';
COMMENT ON COLUMN jl.albp IS 'Additional label path';
COMMENT ON COLUMN jl.albn IS 'Additional label name';
CREATE SEQUENCE jp_bookmark_seq
    START WITH 2954361355555045632
    INCREMENT BY 1
    MINVALUE 2954361355555045632
    MAXVALUE 2990390152574009343
    CACHE 1;
ALTER TABLE jp_bookmark_seq OWNER TO ncuser;
CREATE TABLE jp (
    _bookmark_ bigint DEFAULT nextval('jp_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    csid character varying,
    jpid character varying,
    seqn character varying,
    orjt character varying,
    stat character varying(1),
    crus character varying,
    type character varying(1),
    styp character varying,
    qunt numeric(13,3),
    uprc numeric(19,3),
    pric numeric(19,3),
    crdt date,
    crtm character varying,
    dlus character varying,
    dldt date,
    dltm character varying,
    soid character varying,
    lgid character varying,
    text character varying,
    txam numeric(12,3),
    sltx character varying(1),
    lock character varying(1),
    bass character varying,
    meth character varying,
    munt character varying,
    objt character varying,
    txcd character varying,
    gamt numeric(19,3),
    trat numeric(7,3),
    plac character varying,
    cuid character varying,
    pri2 numeric(19,3),
    txa2 numeric(19,3),
    gam2 numeric(19,3),
    erat numeric(13,4),
    cui2 character varying,
    upr2 numeric(19,3),
    chpc character varying(1),
    wegt numeric(8,3),
    tict numeric(8,3),
    wgrt numeric(8,3),
    twct numeric(8,3),
    calc character varying,
    vsno integer
);
ALTER TABLE jp OWNER TO ncuser;
COMMENT ON TABLE jp IS 'Job Price File';
COMMENT ON COLUMN jp.crcd IS 'Courier Code';
COMMENT ON COLUMN jp.csid IS 'Job id';
COMMENT ON COLUMN jp.jpid IS 'Job price id';
COMMENT ON COLUMN jp.seqn IS 'Sequence Number';
COMMENT ON COLUMN jp.orjt IS 'Original transaction id';
COMMENT ON COLUMN jp.stat IS 'Status';
COMMENT ON COLUMN jp.crus IS 'Created user id';
COMMENT ON COLUMN jp.type IS 'Type';
COMMENT ON COLUMN jp.styp IS 'Source type';
COMMENT ON COLUMN jp.qunt IS 'Quantity';
COMMENT ON COLUMN jp.uprc IS 'Unit Price';
COMMENT ON COLUMN jp.pric IS 'Total price (net)';
COMMENT ON COLUMN jp.crdt IS 'Created date';
COMMENT ON COLUMN jp.crtm IS 'Created Time';
COMMENT ON COLUMN jp.dlus IS 'Deleted user id';
COMMENT ON COLUMN jp.dldt IS 'Deleted date';
COMMENT ON COLUMN jp.dltm IS 'Deleted time';
COMMENT ON COLUMN jp.soid IS 'Source id';
COMMENT ON COLUMN jp.lgid IS 'Leg id';
COMMENT ON COLUMN jp.text IS 'Text';
COMMENT ON COLUMN jp.txam IS 'Tax amount';
COMMENT ON COLUMN jp.sltx IS 'Sales tax applicable';
COMMENT ON COLUMN jp.lock IS 'Locked';
COMMENT ON COLUMN jp.bass IS 'Pricing base';
COMMENT ON COLUMN jp.meth IS 'Method of application';
COMMENT ON COLUMN jp.munt IS 'Measurement unit';
COMMENT ON COLUMN jp.objt IS 'Object name';
COMMENT ON COLUMN jp.txcd IS 'Tax code';
COMMENT ON COLUMN jp.gamt IS 'Gross amount';
COMMENT ON COLUMN jp.trat IS 'Tax rate';
COMMENT ON COLUMN jp.plac IS 'Price line analysis code';
COMMENT ON COLUMN jp.cuid IS 'Currency ';
COMMENT ON COLUMN jp.pri2 IS 'Price in 2nd currency';
COMMENT ON COLUMN jp.txa2 IS 'Tax in 2nd currency';
COMMENT ON COLUMN jp.gam2 IS 'Gross in 2nd currency';
COMMENT ON COLUMN jp.erat IS 'Exchange rate';
COMMENT ON COLUMN jp.cui2 IS '2nd Currency';
COMMENT ON COLUMN jp.upr2 IS 'Unit price in 2nd Currency';
COMMENT ON COLUMN jp.chpc IS 'Charge by percent flag';
COMMENT ON COLUMN jp.wegt IS 'Weight';
COMMENT ON COLUMN jp.tict IS 'Total Item cost';
COMMENT ON COLUMN jp.wgrt IS 'Weight Rate';
COMMENT ON COLUMN jp.twct IS 'Total weight cost';
COMMENT ON COLUMN jp.calc IS 'Calculation details';
COMMENT ON COLUMN jp.vsno IS 'Version No.';
CREATE SEQUENCE keys_bookmark_seq
    START WITH 288230376151712000
    INCREMENT BY 1
    MINVALUE 288230376151712000
    MAXVALUE 324259173170675711
    CACHE 1;
ALTER TABLE keys_bookmark_seq OWNER TO ncuser;
CREATE TABLE keys (
    _bookmark_ bigint DEFAULT nextval('keys_bookmark_seq'::regclass) NOT NULL,
    name character varying,
    fname character varying,
    title text,
    tlen integer,
    num integer,
    fld1 character varying,
    fld2 character varying,
    fld3 character varying,
    fld4 character varying,
    fld5 character varying,
    fld6 character varying,
    fld7 character varying,
    fld8 character varying,
    fld9 character varying,
    fld10 character varying,
    fnum integer
);
ALTER TABLE keys OWNER TO ncuser;
COMMENT ON TABLE keys IS 'System File 8 - Keys';
COMMENT ON COLUMN keys.name IS '[8.1] Key name';
COMMENT ON COLUMN keys.fname IS '[8.2] Logical file name';
COMMENT ON COLUMN keys.title IS '[8.3] Description';
COMMENT ON COLUMN keys.tlen IS '[8.4] Truncated length';
COMMENT ON COLUMN keys.num IS '[8.5] Key number';
COMMENT ON COLUMN keys.fld1 IS '[8.6] Field 1';
COMMENT ON COLUMN keys.fld2 IS '[8.7] Field 2';
COMMENT ON COLUMN keys.fld3 IS '[8.8] Field 3';
COMMENT ON COLUMN keys.fld4 IS '[8.9] Field 4';
COMMENT ON COLUMN keys.fld5 IS '[8.10] Field 5';
COMMENT ON COLUMN keys.fld6 IS '[8.11] Field 6';
COMMENT ON COLUMN keys.fld7 IS '[8.12] Field 7';
COMMENT ON COLUMN keys.fld8 IS '[8.13] Field 8';
COMMENT ON COLUMN keys.fld9 IS '[8.14] Field 9';
COMMENT ON COLUMN keys.fld10 IS '[8.15] Field 10';
COMMENT ON COLUMN keys.fnum IS '[8.16] File no.';
CREATE SEQUENCE ld_bookmark_seq
    START WITH 3891110078048108800
    INCREMENT BY 1
    MINVALUE 3891110078048108800
    MAXVALUE 3927138875067072511
    CACHE 1;
ALTER TABLE ld_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE lf_bookmark_seq
    START WITH 3026418949592973568
    INCREMENT BY 1
    MINVALUE 3026418949592973568
    MAXVALUE 3062447746611937279
    CACHE 1;
ALTER TABLE lf_bookmark_seq OWNER TO ncuser;
CREATE TABLE lf (
    _bookmark_ bigint DEFAULT nextval('lf_bookmark_seq'::regclass) NOT NULL,
    bawb character varying,
    crcd character varying,
    invr character varying,
    lfcd character varying,
    pamf character varying,
    pawb character varying,
    podr character varying,
    pxab character varying,
    seqn character varying,
    stat character varying(1),
    type character varying,
    vsno integer,
    lfid character varying,
    cobf character varying,
    pinf character varying,
    bicu character varying,
    sicu character varying,
    emlr character varying,
    clmf character varying
);
ALTER TABLE lf OWNER TO ncuser;
COMMENT ON TABLE lf IS 'Layout File name';
COMMENT ON COLUMN lf.bawb IS 'Blank AWB File';
COMMENT ON COLUMN lf.crcd IS 'Courier Code';
COMMENT ON COLUMN lf.invr IS 'Invoice report file';
COMMENT ON COLUMN lf.lfcd IS 'Parent id';
COMMENT ON COLUMN lf.pamf IS 'Pre alert manifest file';
COMMENT ON COLUMN lf.pawb IS 'Pre printed awb';
COMMENT ON COLUMN lf.podr IS 'POD Report file';
COMMENT ON COLUMN lf.pxab IS 'Pre-printed without awb';
COMMENT ON COLUMN lf.seqn IS 'Sequence Number';
COMMENT ON COLUMN lf.stat IS 'Status';
COMMENT ON COLUMN lf.type IS 'Type of parent';
COMMENT ON COLUMN lf.vsno IS 'Version No.';
COMMENT ON COLUMN lf.lfid IS 'Layout File Name Id';
COMMENT ON COLUMN lf.cobf IS 'Client Online Booking File';
COMMENT ON COLUMN lf.pinf IS 'Pro forma Invoice File';
COMMENT ON COLUMN lf.bicu IS 'Big Icon URL';
COMMENT ON COLUMN lf.sicu IS 'Small Icon URL';
COMMENT ON COLUMN lf.emlr IS 'Email invoice report file';
COMMENT ON COLUMN lf.clmf IS 'Client online manifest file';
CREATE SEQUENCE lg_bookmark_seq
    START WITH 828662331436171520
    INCREMENT BY 1
    MINVALUE 828662331436171520
    MAXVALUE 864691128455135231
    CACHE 1;
ALTER TABLE lg_bookmark_seq OWNER TO ncuser;
CREATE TABLE lg (
    _bookmark_ bigint DEFAULT nextval('lg_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    lgid character varying,
    lgcd character varying,
    "DESC" character varying,
    stat character varying(1),
    vsno integer,
    pdid character varying,
    wfid character varying,
    fpid character varying,
    tpid character varying,
    suid character varying,
    dvfr date,
    dvto date,
    osat character varying(1),
    osun character varying(1),
    omon character varying(1),
    otue character varying(1),
    owed character varying(1),
    othu character varying(1),
    ofri character varying(1),
    tcut character varying,
    etdp character varying,
    etav character varying,
    trnd integer,
    dsnt character varying,
    trnt character varying,
    trck character varying(1),
    pcid character varying,
    eids character varying,
    type character varying,
    ctyp character varying(1),
    cdes character varying,
    spin character varying,
    mfst character varying,
    slid character varying,
    fzid character varying,
    tzid character varying,
    cntp character varying,
    prio character varying,
    amsf character varying(1),
    bwgt numeric(7,2),
    autb character varying(1),
    mdpt integer,
    mtmp character varying,
    drnm character varying,
    drmf character varying,
    depo character varying,
    drid character varying,
    rtds character varying,
    prod character varying,
    pckg character varying,
    serv character varying,
    otdt character varying,
    cadr character varying,
    dadr character varying,
    optm character varying,
    dtet character varying,
    ctet character varying,
    tmod character varying
);
ALTER TABLE lg OWNER TO ncuser;
COMMENT ON TABLE lg IS 'Routing cost leg';
COMMENT ON COLUMN lg.crcd IS 'Profile Code';
COMMENT ON COLUMN lg.lgid IS 'Leg ID';
COMMENT ON COLUMN lg.lgcd IS 'Leg code';
COMMENT ON COLUMN lg."DESC" IS 'Leg description';
COMMENT ON COLUMN lg.stat IS 'Status';
COMMENT ON COLUMN lg.vsno IS 'Version no';
COMMENT ON COLUMN lg.pdid IS 'Product Id';
COMMENT ON COLUMN lg.wfid IS 'Workflow/Leg type';
COMMENT ON COLUMN lg.fpid IS 'From place Id';
COMMENT ON COLUMN lg.tpid IS 'To place Id';
COMMENT ON COLUMN lg.suid IS 'Supplier Id';
COMMENT ON COLUMN lg.dvfr IS 'Date valid from';
COMMENT ON COLUMN lg.dvto IS 'Date valid to';
COMMENT ON COLUMN lg.osat IS 'Operates on Saturday';
COMMENT ON COLUMN lg.osun IS 'Operates on Sunday';
COMMENT ON COLUMN lg.omon IS 'Operates on Monday';
COMMENT ON COLUMN lg.otue IS 'Operates on Tuesday';
COMMENT ON COLUMN lg.owed IS 'Operates on Wednesday';
COMMENT ON COLUMN lg.othu IS 'Operates on Thursday';
COMMENT ON COLUMN lg.ofri IS 'Operates on Friday';
COMMENT ON COLUMN lg.tcut IS 'Time of cut-off';
COMMENT ON COLUMN lg.etdp IS 'Time of departure';
COMMENT ON COLUMN lg.etav IS 'Time of arrival';
COMMENT ON COLUMN lg.trnd IS 'Transit day';
COMMENT ON COLUMN lg.dsnt IS 'Dispatch notes';
COMMENT ON COLUMN lg.trnt IS 'Tracking note';
COMMENT ON COLUMN lg.trck IS 'Track this';
COMMENT ON COLUMN lg.pcid IS 'Price chart Id';
COMMENT ON COLUMN lg.eids IS 'Extra Ids';
COMMENT ON COLUMN lg.type IS 'Leg type';
COMMENT ON COLUMN lg.ctyp IS 'Cost type';
COMMENT ON COLUMN lg.cdes IS 'Cost type description';
COMMENT ON COLUMN lg.spin IS 'Special instruction';
COMMENT ON COLUMN lg.mfst IS 'Manifest name';
COMMENT ON COLUMN lg.slid IS 'Service level';
COMMENT ON COLUMN lg.fzid IS 'From zone';
COMMENT ON COLUMN lg.tzid IS 'To Zone';
COMMENT ON COLUMN lg.cntp IS 'Content type id';
COMMENT ON COLUMN lg.prio IS 'Priority';
COMMENT ON COLUMN lg.amsf IS 'Send AMS Data?';
COMMENT ON COLUMN lg.bwgt IS 'Maximum bag weight allowed';
COMMENT ON COLUMN lg.autb IS 'Auto manage bag number';
COMMENT ON COLUMN lg.mdpt IS 'Minutes to depot';
COMMENT ON COLUMN lg.mtmp IS 'Manifest Template';
COMMENT ON COLUMN lg.drnm IS 'Driver name';
COMMENT ON COLUMN lg.drmf IS 'Last driver manifest report link';
COMMENT ON COLUMN lg.depo IS 'Depot id';
COMMENT ON COLUMN lg.drid IS 'Driver id';
COMMENT ON COLUMN lg.rtds IS 'Route description';
COMMENT ON COLUMN lg.prod IS 'Product';
COMMENT ON COLUMN lg.pckg IS 'Package type';
COMMENT ON COLUMN lg.serv IS 'Service type';
COMMENT ON COLUMN lg.otdt IS 'Other data';
COMMENT ON COLUMN lg.cadr IS 'Collection address rule';
COMMENT ON COLUMN lg.dadr IS 'Delivery address rule';
COMMENT ON COLUMN lg.optm IS 'Open time';
COMMENT ON COLUMN lg.dtet IS 'Delivery time estimation text';
COMMENT ON COLUMN lg.ctet IS 'Collection time estimation text';
COMMENT ON COLUMN lg.tmod IS 'Transport mode';
CREATE SEQUENCE lh_bookmark_seq
    START WITH 4071254063142928640
    INCREMENT BY 1
    MINVALUE 4071254063142928640
    MAXVALUE 4107282860161892351
    CACHE 1;
ALTER TABLE lh_bookmark_seq OWNER TO ncuser;
CREATE TABLE lh (
    _bookmark_ bigint DEFAULT nextval('lh_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    lhid character varying,
    lhcd character varying,
    "DESC" character varying,
    stat character varying(1),
    vsno integer,
    prm1 character varying,
    prm2 character varying,
    prm3 character varying,
    prm4 character varying,
    prm5 character varying,
    prm6 character varying,
    note character varying,
    prm7 character varying,
    prm8 character varying,
    info character varying,
    prm9 character varying
);
ALTER TABLE lh OWNER TO ncuser;
COMMENT ON TABLE lh IS 'List header file';
COMMENT ON COLUMN lh.crcd IS 'Courier code';
COMMENT ON COLUMN lh.lhid IS 'Unique id';
COMMENT ON COLUMN lh.lhcd IS 'List prefix';
COMMENT ON COLUMN lh."DESC" IS 'Description';
COMMENT ON COLUMN lh.stat IS 'Status';
COMMENT ON COLUMN lh.vsno IS 'Version no.';
COMMENT ON COLUMN lh.prm1 IS 'Prompt 1';
COMMENT ON COLUMN lh.prm2 IS 'Prompt 2';
COMMENT ON COLUMN lh.prm3 IS 'Prompt 3';
COMMENT ON COLUMN lh.prm4 IS 'Prompt 4';
COMMENT ON COLUMN lh.prm5 IS 'Prompt 5';
COMMENT ON COLUMN lh.prm6 IS 'Prompt 6';
COMMENT ON COLUMN lh.note IS 'Note';
COMMENT ON COLUMN lh.prm7 IS 'Prompt 7';
COMMENT ON COLUMN lh.prm8 IS 'Prompt 8';
COMMENT ON COLUMN lh.info IS 'Information';
COMMENT ON COLUMN lh.prm9 IS 'Prompt 9';
CREATE SEQUENCE lo_bookmark_seq
    START WITH 1044835113549955328
    INCREMENT BY 1
    MINVALUE 1044835113549955328
    MAXVALUE 1080863910568919039
    CACHE 1;
ALTER TABLE lo_bookmark_seq OWNER TO ncuser;
CREATE TABLE lo (
    _bookmark_ bigint DEFAULT nextval('lo_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    loid character varying,
    locd character varying,
    "DESC" character varying,
    type character varying(1),
    stat character varying(1),
    vsno integer,
    utx1 character varying,
    tags character varying,
    mvfr character varying(1),
    mvto character varying(1),
    adjs character varying(1),
    pseq integer,
    utx2 character varying
);
ALTER TABLE lo OWNER TO ncuser;
COMMENT ON TABLE lo IS 'Location';
COMMENT ON COLUMN lo.crcd IS 'Courier code';
COMMENT ON COLUMN lo.loid IS 'Location id Id';
COMMENT ON COLUMN lo.locd IS 'Location code';
COMMENT ON COLUMN lo."DESC" IS 'Description';
COMMENT ON COLUMN lo.type IS 'Location type';
COMMENT ON COLUMN lo.stat IS 'Status';
COMMENT ON COLUMN lo.vsno IS 'Version No.';
COMMENT ON COLUMN lo.utx1 IS 'User defined text 1';
COMMENT ON COLUMN lo.tags IS 'Tags';
COMMENT ON COLUMN lo.mvfr IS 'Move from here';
COMMENT ON COLUMN lo.mvto IS 'Move to here';
COMMENT ON COLUMN lo.adjs IS 'Adjust quantity';
COMMENT ON COLUMN lo.pseq IS 'Picking sequence';
COMMENT ON COLUMN lo.utx2 IS 'User defined text 2';
CREATE SEQUENCE ls_bookmark_seq
    START WITH 2522015791327478016
    INCREMENT BY 1
    MINVALUE 2522015791327478016
    MAXVALUE 2558044588346441727
    CACHE 1;
ALTER TABLE ls_bookmark_seq OWNER TO ncuser;
CREATE TABLE ls (
    _bookmark_ bigint DEFAULT nextval('ls_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    lsid character varying,
    lspx character varying,
    crid character varying,
    "DESC" character varying,
    lsvl character varying,
    note character varying,
    stat character varying(1),
    vsno integer,
    seqn character varying,
    exv1 character varying,
    exv2 character varying,
    exv3 character varying,
    exv4 character varying,
    exv5 character varying,
    exv6 character varying,
    exv7 character varying,
    exv8 character varying,
    exv9 character varying,
    ex10 character varying,
    ex11 character varying,
    ex12 character varying,
    ex13 character varying,
    ex14 character varying,
    ex15 character varying,
    ex16 character varying,
    ex17 character varying
);
ALTER TABLE ls OWNER TO ncuser;
COMMENT ON TABLE ls IS 'List File';
COMMENT ON COLUMN ls.crcd IS 'Group Profile';
COMMENT ON COLUMN ls.lsid IS 'List Id';
COMMENT ON COLUMN ls.lspx IS 'List Prefix';
COMMENT ON COLUMN ls.crid IS 'Office Id';
COMMENT ON COLUMN ls."DESC" IS 'Description of list type';
COMMENT ON COLUMN ls.lsvl IS 'Any other Special Value';
COMMENT ON COLUMN ls.note IS 'Note';
COMMENT ON COLUMN ls.stat IS 'Status';
COMMENT ON COLUMN ls.vsno IS 'Version No.';
COMMENT ON COLUMN ls.seqn IS 'Sequence.';
COMMENT ON COLUMN ls.exv1 IS 'Extra value 1';
COMMENT ON COLUMN ls.exv2 IS 'Extra value 2';
COMMENT ON COLUMN ls.exv3 IS 'Extra value 3';
COMMENT ON COLUMN ls.exv4 IS 'Extra value 4';
COMMENT ON COLUMN ls.exv5 IS 'Extra value 5';
COMMENT ON COLUMN ls.exv6 IS 'Extra value 6';
COMMENT ON COLUMN ls.exv7 IS 'Extra value 7';
COMMENT ON COLUMN ls.exv8 IS 'Extra value 8';
COMMENT ON COLUMN ls.exv9 IS 'Extra value 9';
COMMENT ON COLUMN ls.ex10 IS 'Extra value 10';
COMMENT ON COLUMN ls.ex11 IS 'Extra value 11';
COMMENT ON COLUMN ls.ex12 IS 'Extra value 12';
COMMENT ON COLUMN ls.ex13 IS 'Extra value 13';
COMMENT ON COLUMN ls.ex14 IS 'Extra value 14';
COMMENT ON COLUMN ls.ex15 IS 'Extra value 15';
COMMENT ON COLUMN ls.ex16 IS 'Extra value 16';
COMMENT ON COLUMN ls.ex17 IS 'Extra value 17';
CREATE SEQUENCE ma_bookmark_seq
    START WITH 3278620528725721344
    INCREMENT BY 1
    MINVALUE 3278620528725721344
    MAXVALUE 3314649325744685055
    CACHE 1;
ALTER TABLE ma_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE md_bookmark_seq
    START WITH 2810246167479189760
    INCREMENT BY 1
    MINVALUE 2810246167479189760
    MAXVALUE 2846274964498153471
    CACHE 1;
ALTER TABLE md_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE me_bookmark_seq
    START WITH 2882303761517117696
    INCREMENT BY 1
    MINVALUE 2882303761517117696
    MAXVALUE 2918332558536081407
    CACHE 1;
ALTER TABLE me_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE menu_bookmark_seq
    START WITH 432345564227567616
    INCREMENT BY 1
    MINVALUE 432345564227567616
    MAXVALUE 468374361246531583
    CACHE 1;
ALTER TABLE menu_bookmark_seq OWNER TO ncuser;
CREATE TABLE menu (
    _bookmark_ bigint DEFAULT nextval('menu_bookmark_seq'::regclass) NOT NULL,
    levl integer,
    optn integer,
    title character varying,
    trgtn character varying,
    pswd character varying,
    grps character varying
);
ALTER TABLE menu OWNER TO ncuser;
COMMENT ON TABLE menu IS 'System File 12 - Menu definition';
COMMENT ON COLUMN menu.levl IS '[12.1] Menu level number';
COMMENT ON COLUMN menu.optn IS '[12.2] Option number within level';
COMMENT ON COLUMN menu.title IS '[12.3] Description';
COMMENT ON COLUMN menu.trgtn IS '[12.4] Target name';
COMMENT ON COLUMN menu.pswd IS '[12.5] Password';
COMMENT ON COLUMN menu.grps IS '[12.6] Groups';
CREATE SEQUENCE mf_bookmark_seq
    START WITH 1441151880758558976
    INCREMENT BY 1
    MINVALUE 1441151880758558976
    MAXVALUE 1477180677777522687
    CACHE 1;
ALTER TABLE mf_bookmark_seq OWNER TO ncuser;
CREATE TABLE mf (
    _bookmark_ bigint DEFAULT nextval('mf_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    mfid character varying,
    mfcd character varying,
    open character varying(1),
    lgid character varying,
    lgcd character varying,
    suid character varying,
    fpid character varying,
    tpid character varying,
    mfdt date,
    ctyp character varying(1),
    cpcs character varying(2),
    crus character varying,
    crdt date,
    crtm character varying,
    cldt date,
    cltm character varying,
    clus character varying,
    dldt date,
    dltm character varying,
    dlus character varying,
    bagn integer,
    stat character varying(1),
    vsno integer,
    spin character varying,
    cdes character varying,
    cntp character varying,
    jwgt character varying,
    dwgt character varying,
    swgt character varying,
    ecos numeric(11,2),
    ecoc numeric(11,2),
    acos numeric(11,2),
    acoc numeric(11,2),
    etad date,
    etat character varying,
    pdid character varying,
    refr character varying,
    fdet character varying,
    husr character varying,
    ldal date,
    ldat character varying,
    drnm character varying,
    tmid character varying,
    amss character varying(1),
    piid character varying,
    aums character varying(1),
    ltyp character varying,
    etim character varying,
    tmod character varying,
    plin character varying,
    tbat character varying,
    tlbl character varying
);
ALTER TABLE mf OWNER TO ncuser;
COMMENT ON TABLE mf IS 'Manifest File';
COMMENT ON COLUMN mf.crcd IS 'Courier Code';
COMMENT ON COLUMN mf.mfid IS 'Manifest id';
COMMENT ON COLUMN mf.mfcd IS 'Manifest name(MAWB)';
COMMENT ON COLUMN mf.open IS 'Open flag';
COMMENT ON COLUMN mf.lgid IS 'Leg id';
COMMENT ON COLUMN mf.lgcd IS 'Leg code';
COMMENT ON COLUMN mf.suid IS 'Supplier id';
COMMENT ON COLUMN mf.fpid IS 'Origin place Id';
COMMENT ON COLUMN mf.tpid IS 'Destination place Id';
COMMENT ON COLUMN mf.mfdt IS 'Manifest date';
COMMENT ON COLUMN mf.ctyp IS 'Cost type (I/C)';
COMMENT ON COLUMN mf.cpcs IS 'Re-cost (Y/N)';
COMMENT ON COLUMN mf.crus IS 'Created user id';
COMMENT ON COLUMN mf.crdt IS 'Opening date';
COMMENT ON COLUMN mf.crtm IS 'Created time';
COMMENT ON COLUMN mf.cldt IS 'Closing Date';
COMMENT ON COLUMN mf.cltm IS 'Closing time ';
COMMENT ON COLUMN mf.clus IS 'User Closed';
COMMENT ON COLUMN mf.dldt IS 'Deleted date';
COMMENT ON COLUMN mf.dltm IS 'Deleted time';
COMMENT ON COLUMN mf.dlus IS 'Deleted user id';
COMMENT ON COLUMN mf.bagn IS 'Bag Number';
COMMENT ON COLUMN mf.stat IS 'Status';
COMMENT ON COLUMN mf.vsno IS 'Version number';
COMMENT ON COLUMN mf.spin IS 'Special instruction';
COMMENT ON COLUMN mf.cdes IS 'Cost type description';
COMMENT ON COLUMN mf.cntp IS 'Contents type';
COMMENT ON COLUMN mf.jwgt IS 'Job weight';
COMMENT ON COLUMN mf.dwgt IS 'Declare weight';
COMMENT ON COLUMN mf.swgt IS 'Supplier stated weight';
COMMENT ON COLUMN mf.ecos IS 'Estimated cost in supplier currency';
COMMENT ON COLUMN mf.ecoc IS 'Estimated cost in courier currency';
COMMENT ON COLUMN mf.acos IS 'Actual cost in supplier currency';
COMMENT ON COLUMN mf.acoc IS 'Actual cost in courier currency';
COMMENT ON COLUMN mf.etad IS 'ETA date';
COMMENT ON COLUMN mf.etat IS 'ETA time';
COMMENT ON COLUMN mf.pdid IS 'Product id';
COMMENT ON COLUMN mf.refr IS 'Reference id (Mafest code for the leg)';
COMMENT ON COLUMN mf.fdet IS 'Flight details';
COMMENT ON COLUMN mf.husr IS 'Handheld user';
COMMENT ON COLUMN mf.ldal IS 'Last driver allocation';
COMMENT ON COLUMN mf.ldat IS 'Last driver allocation time';
COMMENT ON COLUMN mf.drnm IS 'Driver name';
COMMENT ON COLUMN mf.tmid IS 'TMID of most recent transm';
COMMENT ON COLUMN mf.amss IS 'AMS Data sent?';
COMMENT ON COLUMN mf.piid IS 'Purchase Invoice Id';
COMMENT ON COLUMN mf.aums IS 'Auto manifest sent?';
COMMENT ON COLUMN mf.ltyp IS 'Manifest type';
COMMENT ON COLUMN mf.etim IS 'Estimated end time';
COMMENT ON COLUMN mf.tmod IS 'Transport mode';
COMMENT ON COLUMN mf.plin IS 'Poly line';
COMMENT ON COLUMN mf.tbat IS 'Thirdparty MF batch no';
COMMENT ON COLUMN mf.tlbl IS 'Thirdparty MF label DC id';
CREATE SEQUENCE mo_bookmark_seq
    START WITH 2161727821137838336
    INCREMENT BY 1
    MINVALUE 2161727821137838336
    MAXVALUE 2197756618156802047
    CACHE 1;
ALTER TABLE mo_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE mq_bookmark_seq
    START WITH 4215369251218784512
    INCREMENT BY 1
    MINVALUE 4215369251218784512
    MAXVALUE 4251398048237748223
    CACHE 1;
ALTER TABLE mq_bookmark_seq OWNER TO ncuser;
CREATE TABLE mq (
    _bookmark_ bigint DEFAULT nextval('mq_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    mqid character varying,
    keyn character varying,
    keyv character varying,
    flag character varying(1),
    lcdt date,
    lctm character varying,
    ludt date,
    lutm character varying
);
ALTER TABLE mq OWNER TO ncuser;
COMMENT ON TABLE mq IS 'Mirror queue file';
COMMENT ON COLUMN mq.crcd IS 'Courier code';
COMMENT ON COLUMN mq.mqid IS 'Mirrir id';
COMMENT ON COLUMN mq.keyn IS 'Key name';
COMMENT ON COLUMN mq.keyv IS 'Key value';
COMMENT ON COLUMN mq.flag IS 'Mirrored flag';
COMMENT ON COLUMN mq.lcdt IS 'Last craetetion date';
COMMENT ON COLUMN mq.lctm IS 'Last craetetion time';
COMMENT ON COLUMN mq.ludt IS 'Last export date';
COMMENT ON COLUMN mq.lutm IS 'Last export time';
CREATE SEQUENCE ms_bookmark_seq
    START WITH 3602879701896397056
    INCREMENT BY 1
    MINVALUE 3602879701896397056
    MAXVALUE 3638908498915360767
    CACHE 1;
ALTER TABLE ms_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE nl_bookmark_seq
    START WITH 3134505340649865472
    INCREMENT BY 1
    MINVALUE 3134505340649865472
    MAXVALUE 3170534137668829183
    CACHE 1;
ALTER TABLE nl_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE ns_bookmark_seq
    START WITH 2594073385365405952
    INCREMENT BY 1
    MINVALUE 2594073385365405952
    MAXVALUE 2630102182384369663
    CACHE 1;
ALTER TABLE ns_bookmark_seq OWNER TO ncuser;
CREATE TABLE ns (
    _bookmark_ bigint DEFAULT nextval('ns_bookmark_seq'::regclass) NOT NULL,
    type character varying(1),
    refr character varying,
    opid character varying,
    dnum bigint,
    pfix character varying,
    nnum bigint
);
ALTER TABLE ns OWNER TO ncuser;
COMMENT ON TABLE ns IS 'Number series';
COMMENT ON COLUMN ns.type IS 'Type';
COMMENT ON COLUMN ns.refr IS 'Reference Number';
COMMENT ON COLUMN ns.opid IS 'Office or Product Id';
COMMENT ON COLUMN ns.dnum IS 'No of Digits';
COMMENT ON COLUMN ns.pfix IS 'Prefix';
COMMENT ON COLUMN ns.nnum IS 'Next Number';
CREATE SEQUENCE nt_bookmark_seq
    START WITH 3566850904877433088
    INCREMENT BY 1
    MINVALUE 3566850904877433088
    MAXVALUE 3602879701896396799
    CACHE 1;
ALTER TABLE nt_bookmark_seq OWNER TO ncuser;
CREATE TABLE nt (
    _bookmark_ bigint DEFAULT nextval('nt_bookmark_seq'::regclass) NOT NULL,
    asto character varying,
    crcd character varying,
    csts character varying,
    usid character varying,
    ctdt date,
    cttm character varying,
    doby date,
    dscs character varying(1),
    note character varying,
    ntcd character varying,
    ntid character varying,
    pobj character varying,
    pobn character varying,
    pobv character varying,
    stat character varying(1),
    type character varying(1),
    upby character varying,
    updt date,
    uptm character varying,
    dotm character varying,
    dcol character varying(1),
    robj character varying,
    rurf character varying,
    suid character varying,
    svid character varying,
    ttyp character varying,
    task character varying,
    rmdt date,
    rmtm character varying,
    drmf character varying(1),
    ermf character varying(1),
    rtim bigint,
    dtim bigint,
    fndt date,
    fntm character varying,
    ud01 character varying,
    ud02 character varying,
    ud03 character varying,
    ud04 character varying,
    ud05 character varying,
    ud06 character varying,
    ud07 character varying,
    ud08 character varying,
    ud09 character varying,
    ud10 character varying,
    vsno integer
);
ALTER TABLE nt OWNER TO ncuser;
COMMENT ON TABLE nt IS 'Notes and Tasks File';
COMMENT ON COLUMN nt.asto IS 'Assign To. (for only task)';
COMMENT ON COLUMN nt.crcd IS 'Courier code';
COMMENT ON COLUMN nt.csts IS 'Current Status';
COMMENT ON COLUMN nt.usid IS 'Created by (System User)';
COMMENT ON COLUMN nt.ctdt IS 'Created date';
COMMENT ON COLUMN nt.cttm IS 'Created Time';
COMMENT ON COLUMN nt.doby IS 'Do this Task By ( for task)';
COMMENT ON COLUMN nt.dscs IS 'Display in Job Entry Screen';
COMMENT ON COLUMN nt.note IS 'Description of Note or Task';
COMMENT ON COLUMN nt.ntcd IS 'Parent Object Value';
COMMENT ON COLUMN nt.ntid IS 'Note and task ID';
COMMENT ON COLUMN nt.pobj IS 'Parent Object';
COMMENT ON COLUMN nt.pobn IS 'Parent Object Name';
COMMENT ON COLUMN nt.pobv IS 'Parent name';
COMMENT ON COLUMN nt.stat IS 'Status';
COMMENT ON COLUMN nt.type IS 'Note type';
COMMENT ON COLUMN nt.upby IS 'Last Updated Time (System User';
COMMENT ON COLUMN nt.updt IS 'Last Updated Date';
COMMENT ON COLUMN nt.uptm IS 'Last Updated Time';
COMMENT ON COLUMN nt.dotm IS 'Do this by time( for task)';
COMMENT ON COLUMN nt.dcol IS 'Display in client online';
COMMENT ON COLUMN nt.robj IS 'Rule select type';
COMMENT ON COLUMN nt.rurf IS 'Rule reference id';
COMMENT ON COLUMN nt.suid IS 'Subject id';
COMMENT ON COLUMN nt.svid IS 'Supervisor User Id';
COMMENT ON COLUMN nt.ttyp IS 'Task Type Id';
COMMENT ON COLUMN nt.task IS 'Task Description/name';
COMMENT ON COLUMN nt.rmdt IS 'task reminder date';
COMMENT ON COLUMN nt.rmtm IS 'task reminder time';
COMMENT ON COLUMN nt.drmf IS 'deadline reminder flag';
COMMENT ON COLUMN nt.ermf IS 'email reminder flag';
COMMENT ON COLUMN nt.rtim IS 'Reminder time in minutes';
COMMENT ON COLUMN nt.dtim IS 'Deadline time in minutes';
COMMENT ON COLUMN nt.fndt IS 'Finish date';
COMMENT ON COLUMN nt.fntm IS 'Finish time';
COMMENT ON COLUMN nt.ud01 IS 'User defined field 1';
COMMENT ON COLUMN nt.ud02 IS 'User defined field 2';
COMMENT ON COLUMN nt.ud03 IS 'User defined field 3';
COMMENT ON COLUMN nt.ud04 IS 'User defined field 4';
COMMENT ON COLUMN nt.ud05 IS 'User defined field 5';
COMMENT ON COLUMN nt.ud06 IS 'User defined field 6';
COMMENT ON COLUMN nt.ud07 IS 'User defined field 7';
COMMENT ON COLUMN nt.ud08 IS 'User defined field 8';
COMMENT ON COLUMN nt.ud09 IS 'User defined field 9';
COMMENT ON COLUMN nt.ud10 IS 'User defined field 10';
CREATE SEQUENCE number_series_ad_seq
    START WITH 200000070566
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_ad_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_af_seq
    START WITH 20000005
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_af_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_an_seq
    START WITH 10000044
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_an_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ap_seq
    START WITH 20000046
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_ap_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ar_seq
    START WITH 10026428
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_ar_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_at_seq
    START WITH 20000000
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_at_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_au_seq
    START WITH 200000090977
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_au_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_bp_seq
    START WITH 20006461
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_bp_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_bt_seq
    START WITH 200000008248
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_bt_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_cd_seq
    START WITH 200004137082
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_cd_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ci_seq
    START WITH 220000031542
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_ci_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_cj_seq
    START WITH 10000008
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_cj_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_cl_seq
    START WITH 10013149
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_cl_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_co_seq
    START WITH 10000277
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_co_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_cp_seq
    START WITH 100000002195
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_cp_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_cs_seq
    START WITH 100000093581
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_cs_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ct_seq
    START WITH 10020029
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_ct_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_cu_seq
    START WITH 10003136
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_cu_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_cv_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE number_series_cv_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_cy_seq
    START WITH 10000393
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_cy_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_dc_seq
    START WITH 100000085573
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_dc_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_de_seq
    START WITH 10026322
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_de_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_di_seq
    START WITH 200026140003
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_di_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_dt_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE number_series_dt_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ea_seq
    START WITH 100000000314
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_ea_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ex_seq
    START WITH 100647914
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 9999999999
    CACHE 1;
ALTER TABLE number_series_ex_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_fd_seq
    START WITH 11000211
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_fd_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_fl_seq
    START WITH 10000281
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_fl_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_fr_seq
    START WITH 10000052
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_fr_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ho_seq
    START WITH 20000375
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_ho_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ia_seq
    START WITH 10000000
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_ia_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ip_seq
    START WITH 100000000021
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_ip_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_it_seq
    START WITH 10001080
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_it_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ja_seq
    START WITH 100000000000
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_ja_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_jd_seq
    START WITH 100000000000
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_jd_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_jl_seq
    START WITH 100000018362
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_jl_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_jp_seq
    START WITH 100000097130
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_jp_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_lf_seq
    START WITH 10000031
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_lf_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_lg_seq
    START WITH 10000307
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_lg_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_lh_seq
    START WITH 10000466
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_lh_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_lo_seq
    START WITH 20000018
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_lo_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ls_seq
    START WITH 10767181
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_ls_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_mf_seq
    START WITH 100000000262
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_mf_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_mm_seq
    START WITH 10001100
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_mm_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_mq_seq
    START WITH 100000391655
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_mq_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ms_seq
    START WITH 10000104
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_ms_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_mt_seq
    START WITH 10000000
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_mt_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_mv_seq
    START WITH 10000000
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_mv_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_nt_seq
    START WITH 10001105
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_nt_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_pa_seq
    START WITH 10042278
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_pa_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_pc_seq
    START WITH 10003879
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_pc_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_pd_seq
    START WITH 10000072
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_pd_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_pe_seq
    START WITH 10001286
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_pe_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_pf_seq
    START WITH 10000005
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_pf_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_pi_seq
    START WITH 10000291
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_pi_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_pl_seq
    START WITH 20010067
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_pl_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_pm_seq
    START WITH 10000029
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_pm_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_po_seq
    START WITH 10000000
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_po_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_pr_seq
    START WITH 10002203
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_pr_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_pz_seq
    START WITH 10000000
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_pz_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_re_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE number_series_re_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_rl_seq
    START WITH 10000548
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_rl_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_rm_seq
    START WITH 10000000
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_rm_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_rp_seq
    START WITH 10001045
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_rp_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_rs_seq
    START WITH 10000000
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_rs_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_rt_seq
    START WITH 10000182
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_rt_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ru_seq
    START WITH 10000020
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_ru_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_sd_seq
    START WITH 10000003
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_sd_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_se_seq
    START WITH 10000000
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_se_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_si_seq
    START WITH 100000000189
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_si_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_sl_seq
    START WITH 10002361
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_sl_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_sm_seq
    START WITH 200000011557
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_sm_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_sq_seq
    START WITH 20001742
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_sq_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_su_seq
    START WITH 10000684
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_su_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_sy_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE number_series_sy_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_test_seq
    START WITH 1038
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999
    CACHE 1;
ALTER TABLE number_series_test_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_tf_seq
    START WITH 100000000124
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_tf_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_tg_seq
    START WITH 9999999
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_tg_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_th_seq
    START WITH 100000549821
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_th_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_tl_seq
    START WITH 100
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999
    CACHE 1;
ALTER TABLE number_series_tl_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_tq_seq
    START WITH 100000002535
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_tq_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_tr_seq
    START WITH 10000887
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_tr_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ts2_seq
    START WITH 443
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999
    CACHE 1;
ALTER TABLE number_series_ts2_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_tst_seq
    START WITH 10000000
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_tst_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_tx_seq
    START WITH 10000039
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_tx_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ua_seq
    START WITH 10000071
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_ua_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_ud_seq
    START WITH 10000003
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_ud_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_us_seq
    START WITH 10000436
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_us_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_we_seq
    START WITH 300000099198
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 999999999999
    CACHE 1;
ALTER TABLE number_series_we_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_wf_seq
    START WITH 10000197
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_wf_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_xs_seq
    START WITH 10001
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999
    CACHE 1;
ALTER TABLE number_series_xs_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_za_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE number_series_za_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_zg_seq
    START WITH 10000038
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_zg_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_zn_seq
    START WITH 10000712
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999999
    CACHE 1;
ALTER TABLE number_series_zn_seq OWNER TO ncuser;
CREATE SEQUENCE number_series_zz_seq
    START WITH 10000
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 99999
    CACHE 1;
ALTER TABLE number_series_zz_seq OWNER TO ncuser;
CREATE SEQUENCE oa_bookmark_seq
    START WITH 2558044588346441984
    INCREMENT BY 1
    MINVALUE 2558044588346441984
    MAXVALUE 2594073385365405695
    CACHE 1;
ALTER TABLE oa_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE om_bookmark_seq
    START WITH 3999196469105000704
    INCREMENT BY 1
    MINVALUE 3999196469105000704
    MAXVALUE 4035225266123964415
    CACHE 1;
ALTER TABLE om_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE or_bookmark_seq
    START WITH 3314649325744685312
    INCREMENT BY 1
    MINVALUE 3314649325744685312
    MAXVALUE 3350678122763649023
    CACHE 1;
ALTER TABLE or_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE pa_bookmark_seq
    START WITH 1549238271815450880
    INCREMENT BY 1
    MINVALUE 1549238271815450880
    MAXVALUE 1585267068834414591
    CACHE 1;
ALTER TABLE pa_bookmark_seq OWNER TO ncuser;
CREATE TABLE pa (
    _bookmark_ bigint DEFAULT nextval('pa_bookmark_seq'::regclass) NOT NULL,
    paid character varying,
    crcd character varying,
    pdid character varying,
    pcid character varying,
    slid character varying,
    ctyp character varying,
    clid character varying,
    fzid character varying,
    tzid character varying,
    prio character varying,
    vsno integer,
    stat character varying(1),
    disc numeric(18,6),
    dtyp character varying(3),
    sdes character varying,
    dvfr date
);
ALTER TABLE pa OWNER TO ncuser;
COMMENT ON TABLE pa IS 'Price Allocation File';
COMMENT ON COLUMN pa.paid IS 'Price allocation Id';
COMMENT ON COLUMN pa.crcd IS 'Courier code';
COMMENT ON COLUMN pa.pdid IS 'Product Id';
COMMENT ON COLUMN pa.pcid IS 'Price chart Id';
COMMENT ON COLUMN pa.slid IS 'Service Level ID';
COMMENT ON COLUMN pa.ctyp IS 'Contents ID';
COMMENT ON COLUMN pa.clid IS 'Client Id';
COMMENT ON COLUMN pa.fzid IS 'Zone from id';
COMMENT ON COLUMN pa.tzid IS 'Zone to id';
COMMENT ON COLUMN pa.prio IS 'Priority';
COMMENT ON COLUMN pa.vsno IS 'Version No.';
COMMENT ON COLUMN pa.stat IS 'Status';
COMMENT ON COLUMN pa.disc IS 'Discount';
COMMENT ON COLUMN pa.dtyp IS 'Discount type PCV-percent MUL for fixed';
COMMENT ON COLUMN pa.sdes IS 'Service description for client';
COMMENT ON COLUMN pa.dvfr IS 'Valid from date';
CREATE SEQUENCE pb_bookmark_seq
    START WITH 1513209474796486912
    INCREMENT BY 1
    MINVALUE 1513209474796486912
    MAXVALUE 1549238271815450623
    CACHE 1;
ALTER TABLE pb_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE pc_bookmark_seq
    START WITH 1585267068834414848
    INCREMENT BY 1
    MINVALUE 1585267068834414848
    MAXVALUE 1621295865853378559
    CACHE 1;
ALTER TABLE pc_bookmark_seq OWNER TO ncuser;
CREATE TABLE pc (
    _bookmark_ bigint DEFAULT nextval('pc_bookmark_seq'::regclass) NOT NULL,
    pcid character varying,
    crcd character varying,
    pccd character varying,
    "DESC" character varying,
    cuid character varying,
    bsut character varying,
    dtut character varying(3),
    stat character varying(1),
    vsno integer,
    suid character varying,
    type character varying(1),
    txcd character varying,
    plac character varying,
    tags character varying,
    chfl character varying,
    chml character varying,
    chpc character varying(1),
    buml character varying,
    pdid character varying,
    acfr date,
    acto date,
    tbun character varying,
    maxd numeric(14,2),
    maxv numeric(14,2),
    fzid character varying,
    tzid character varying,
    prio character varying,
    ctyp character varying,
    tbml character varying,
    bopc character varying(1),
    ttyp character varying,
    ajty character varying,
    ajam numeric(13,2),
    uldl character varying
);
ALTER TABLE pc OWNER TO ncuser;
COMMENT ON TABLE pc IS 'Price Chart File';
COMMENT ON COLUMN pc.pcid IS 'Price chart unique ID';
COMMENT ON COLUMN pc.crcd IS 'Courier Code';
COMMENT ON COLUMN pc.pccd IS 'Price chart code';
COMMENT ON COLUMN pc."DESC" IS 'Price chart name';
COMMENT ON COLUMN pc.cuid IS 'Currency Id';
COMMENT ON COLUMN pc.bsut IS 'Weight Unit';
COMMENT ON COLUMN pc.dtut IS 'Distance Unit';
COMMENT ON COLUMN pc.stat IS 'Record Status';
COMMENT ON COLUMN pc.vsno IS 'Version No.';
COMMENT ON COLUMN pc.suid IS 'Supplier ID';
COMMENT ON COLUMN pc.type IS 'Price Chart Type';
COMMENT ON COLUMN pc.txcd IS 'TAX rate code';
COMMENT ON COLUMN pc.plac IS 'Price line analysis code';
COMMENT ON COLUMN pc.tags IS 'Tags';
COMMENT ON COLUMN pc.chfl IS 'Charge by field';
COMMENT ON COLUMN pc.chml IS 'Charge by multiplier';
COMMENT ON COLUMN pc.chpc IS 'Charge by percent flag';
COMMENT ON COLUMN pc.buml IS 'Base unit multiplier';
COMMENT ON COLUMN pc.pdid IS 'Product Id';
COMMENT ON COLUMN pc.acfr IS 'Active from';
COMMENT ON COLUMN pc.acto IS 'Active to';
COMMENT ON COLUMN pc.tbun IS 'Table band unit';
COMMENT ON COLUMN pc.maxd IS 'Maximum dimension';
COMMENT ON COLUMN pc.maxv IS 'Maximum volume';
COMMENT ON COLUMN pc.fzid IS 'From zone';
COMMENT ON COLUMN pc.tzid IS 'To zone';
COMMENT ON COLUMN pc.prio IS 'Priority';
COMMENT ON COLUMN pc.ctyp IS 'Contents type';
COMMENT ON COLUMN pc.tbml IS 'Table band unit multiplier';
COMMENT ON COLUMN pc.bopc IS 'Back offic use only flag';
COMMENT ON COLUMN pc.ttyp IS 'Tariff type';
COMMENT ON COLUMN pc.ajty IS 'Adjustment type';
COMMENT ON COLUMN pc.ajam IS 'Adjustment value';
COMMENT ON COLUMN pc.uldl IS 'Uplift Downlift';
CREATE SEQUENCE pd_bookmark_seq
    START WITH 3602879701896397056
    INCREMENT BY 1
    MINVALUE 3602879701896397056
    MAXVALUE 3638908498915360767
    CACHE 1;
ALTER TABLE pd_bookmark_seq OWNER TO ncuser;
CREATE TABLE pd (
    _bookmark_ bigint DEFAULT nextval('pd_bookmark_seq'::regclass) NOT NULL,
    pdid character varying,
    crcd character varying,
    pdcd character varying,
    "DESC" character varying,
    vfac numeric(10,4),
    aawb character varying(1),
    pawb character varying,
    lawb integer,
    nawb bigint,
    stat character varying(1),
    vsno integer,
    wunt character varying,
    uhwd character varying,
    dunt character varying,
    bprf character varying,
    prog character varying,
    rege character varying,
    cnfd character varying(4),
    naut character varying(1),
    dawb character varying(1)
);
ALTER TABLE pd OWNER TO ncuser;
COMMENT ON TABLE pd IS 'Product File';
COMMENT ON COLUMN pd.pdid IS 'Unique ID';
COMMENT ON COLUMN pd.crcd IS 'Courier Code';
COMMENT ON COLUMN pd.pdcd IS 'Product Code';
COMMENT ON COLUMN pd."DESC" IS 'Product name';
COMMENT ON COLUMN pd.vfac IS 'Volumetric factor';
COMMENT ON COLUMN pd.aawb IS 'Auto air way bill flag';
COMMENT ON COLUMN pd.pawb IS 'AWB Prefix';
COMMENT ON COLUMN pd.lawb IS 'Length of AWB';
COMMENT ON COLUMN pd.nawb IS 'Next AWB';
COMMENT ON COLUMN pd.stat IS 'Record Status';
COMMENT ON COLUMN pd.vsno IS 'Version No.';
COMMENT ON COLUMN pd.wunt IS 'Unit of weight';
COMMENT ON COLUMN pd.uhwd IS 'Unit of lenght (for weight)';
COMMENT ON COLUMN pd.dunt IS 'Unit of distance';
COMMENT ON COLUMN pd.bprf IS 'Booking preferences id';
COMMENT ON COLUMN pd.prog IS 'Booking program name';
COMMENT ON COLUMN pd.rege IS 'Hawb regular expression';
COMMENT ON COLUMN pd.cnfd IS 'Contents field';
COMMENT ON COLUMN pd.naut IS 'Do not auto allocate flag';
COMMENT ON COLUMN pd.dawb IS 'Disable auto AWB printing';
CREATE SEQUENCE pe_bookmark_seq
    START WITH 3710966092953288960
    INCREMENT BY 1
    MINVALUE 3710966092953288960
    MAXVALUE 3746994889972252671
    CACHE 1;
ALTER TABLE pe_bookmark_seq OWNER TO ncuser;
CREATE TABLE pe (
    _bookmark_ bigint DEFAULT nextval('pe_bookmark_seq'::regclass) NOT NULL,
    peid character varying,
    crcd character varying,
    paid character varying,
    exid character varying,
    stat character varying(1),
    vsno integer
);
ALTER TABLE pe OWNER TO ncuser;
COMMENT ON TABLE pe IS 'Potential extra history';
COMMENT ON COLUMN pe.peid IS 'Unique ID';
COMMENT ON COLUMN pe.crcd IS 'Courier Code';
COMMENT ON COLUMN pe.paid IS 'Price allocation id';
COMMENT ON COLUMN pe.exid IS 'Extra id';
COMMENT ON COLUMN pe.stat IS 'Record Status';
CREATE SEQUENCE pg_bookmark_seq
    START WITH 1549238271815450880
    INCREMENT BY 1
    MINVALUE 1549238271815450880
    MAXVALUE 1585267068834414591
    CACHE 1;
ALTER TABLE pg_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE ph_bookmark_seq
    START WITH 2197756618156802304
    INCREMENT BY 1
    MINVALUE 2197756618156802304
    MAXVALUE 2233785415175766015
    CACHE 1;
ALTER TABLE ph_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE phys_bookmark_seq
    START WITH 72057594037928192
    INCREMENT BY 1
    MINVALUE 72057594037928192
    MAXVALUE 108086391056891903
    CACHE 1;
ALTER TABLE phys_bookmark_seq OWNER TO ncuser;
CREATE TABLE phys (
    _bookmark_ bigint DEFAULT nextval('phys_bookmark_seq'::regclass) NOT NULL,
    pname character varying,
    pnum integer,
    title character varying,
    loc character varying,
    bloksz integer,
    maxrec integer,
    overfl character varying
);
ALTER TABLE phys OWNER TO ncuser;
COMMENT ON TABLE phys IS 'System File 2 - Physical files';
COMMENT ON COLUMN phys.pname IS '[2.1]	Physical file name';
COMMENT ON COLUMN phys.pnum IS '[2.2]	Physical file no.';
COMMENT ON COLUMN phys.title IS '[2.3]	Description';
COMMENT ON COLUMN phys.loc IS '[2.4]	Location';
COMMENT ON COLUMN phys.bloksz IS '[2.5]	Block size';
COMMENT ON COLUMN phys.maxrec IS '[2.6]	Maximum no. of records';
COMMENT ON COLUMN phys.overfl IS '[2.7]	Overflow file name';
CREATE SEQUENCE pi_bookmark_seq
    START WITH 1621295865853378816
    INCREMENT BY 1
    MINVALUE 1621295865853378816
    MAXVALUE 1657324662872342527
    CACHE 1;
ALTER TABLE pi_bookmark_seq OWNER TO ncuser;
CREATE TABLE pi (
    _bookmark_ bigint DEFAULT nextval('pi_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    suid character varying,
    indt date,
    sivn character varying,
    exrt numeric(9,3),
    tisc numeric(11,2),
    ticc numeric(11,2),
    risc numeric(11,2),
    ricc numeric(11,2),
    stat character varying(1),
    vsno integer,
    piid character varying,
    type character varying(1),
    clos character varying(1),
    ntsc numeric(11,2),
    ntcc numeric(11,2),
    txsc numeric(11,2),
    txcc numeric(11,2),
    surc numeric(11,2),
    fdat date,
    ldat date,
    cuid character varying,
    note character varying,
    tags character varying,
    josm character varying,
    dfil character varying,
    dfmt character varying,
    exll numeric(11,2),
    exul numeric(11,2),
    rldv character varying,
    rnpd character varying,
    fusc numeric(11,2),
    fuss numeric(11,2),
    vtyp character varying,
    exto character varying,
    expt character varying(1)
);
ALTER TABLE pi OWNER TO ncuser;
COMMENT ON TABLE pi IS 'Purchase Invoice File';
COMMENT ON COLUMN pi.crcd IS 'Courier Code';
COMMENT ON COLUMN pi.suid IS 'Supplier Id';
COMMENT ON COLUMN pi.indt IS 'Invoice Date';
COMMENT ON COLUMN pi.sivn IS 'Supplier Invoice No';
COMMENT ON COLUMN pi.exrt IS 'Exchange Rate';
COMMENT ON COLUMN pi.tisc IS 'Total Inv Amount in SU Curr.';
COMMENT ON COLUMN pi.ticc IS 'Total Inv Amount in CR Curr.';
COMMENT ON COLUMN pi.risc IS 'Reconciled Amount SU Curr.';
COMMENT ON COLUMN pi.ricc IS 'Reconcile Amount in CR Curr.';
COMMENT ON COLUMN pi.stat IS 'Status';
COMMENT ON COLUMN pi.vsno IS 'Version No.';
COMMENT ON COLUMN pi.piid IS 'Purchase Invoice Id';
COMMENT ON COLUMN pi.type IS 'Purchase Invoice Type';
COMMENT ON COLUMN pi.clos IS 'Invoice close for reconcilliation';
COMMENT ON COLUMN pi.ntsc IS 'Net total in supplier currency';
COMMENT ON COLUMN pi.ntcc IS 'Net total in courier currency';
COMMENT ON COLUMN pi.txsc IS 'Total tax in supplier currency';
COMMENT ON COLUMN pi.txcc IS 'Total tax in courier currency';
COMMENT ON COLUMN pi.surc IS 'Surcharge %';
COMMENT ON COLUMN pi.fdat IS 'First job date';
COMMENT ON COLUMN pi.ldat IS 'Last job date';
COMMENT ON COLUMN pi.cuid IS 'Supplier currency';
COMMENT ON COLUMN pi.note IS 'Notes';
COMMENT ON COLUMN pi.tags IS 'Tags';
COMMENT ON COLUMN pi.josm IS 'Job search method';
COMMENT ON COLUMN pi.dfil IS 'Import data file';
COMMENT ON COLUMN pi.dfmt IS 'Data formate';
COMMENT ON COLUMN pi.exll IS 'Exchange rate lower limit';
COMMENT ON COLUMN pi.exul IS 'Exchange rate upper limit';
COMMENT ON COLUMN pi.rldv IS 'Reject late delivery';
COMMENT ON COLUMN pi.rnpd IS 'Reject no PoD';
COMMENT ON COLUMN pi.fusc IS 'Fuel surcharge courier';
COMMENT ON COLUMN pi.fuss IS 'Fuel surcharge invoice';
COMMENT ON COLUMN pi.vtyp IS 'Variance type(F-fixed P-percentage)';
COMMENT ON COLUMN pi.exto IS 'Exported to';
COMMENT ON COLUMN pi.expt IS 'Exported to accounts system';
CREATE SEQUENCE pl_bookmark_seq
    START WITH 3458764513820541184
    INCREMENT BY 1
    MINVALUE 3458764513820541184
    MAXVALUE 3494793310839504895
    CACHE 1;
ALTER TABLE pl_bookmark_seq OWNER TO ncuser;
CREATE TABLE pl (
    _bookmark_ bigint DEFAULT nextval('pl_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    plid character varying,
    "DESC" character varying,
    type character varying(2),
    coid character varying,
    latt numeric(8,2),
    lont numeric(8,2),
    adr1 character varying,
    adr2 character varying,
    adr3 character varying,
    adr4 character varying,
    psfr character varying,
    psto character varying,
    stat character varying(1),
    vsno integer,
    altn character varying,
    tdes character varying,
    tags character varying,
    weig integer,
    zpcd character varying,
    search_phrase tsvector,
    cocd character varying,
    countryname character varying,
    psfr_lpad character varying,
    altn_lpad character varying
);
ALTER TABLE pl OWNER TO ncuser;
COMMENT ON TABLE pl IS 'Places file';
COMMENT ON COLUMN pl.crcd IS 'Courier code';
COMMENT ON COLUMN pl.plid IS 'Unique id';
COMMENT ON COLUMN pl."DESC" IS 'Name';
COMMENT ON COLUMN pl.type IS 'Place type';
COMMENT ON COLUMN pl.coid IS 'Country id';
COMMENT ON COLUMN pl.latt IS 'Latitude';
COMMENT ON COLUMN pl.lont IS 'Longitude';
COMMENT ON COLUMN pl.adr1 IS 'Address Line 1';
COMMENT ON COLUMN pl.adr2 IS 'Address Line 2';
COMMENT ON COLUMN pl.adr3 IS 'Address Line 3';
COMMENT ON COLUMN pl.adr4 IS 'Address Line 4';
COMMENT ON COLUMN pl.psfr IS 'Post Code from';
COMMENT ON COLUMN pl.psto IS 'Post Code to (not in use)';
COMMENT ON COLUMN pl.stat IS 'Status';
COMMENT ON COLUMN pl.vsno IS 'Version No.';
COMMENT ON COLUMN pl.altn IS 'Alternative name (post to)';
COMMENT ON COLUMN pl.tdes IS 'Type description';
COMMENT ON COLUMN pl.tags IS 'Tags';
COMMENT ON COLUMN pl.weig IS 'Weigth for display/search';
COMMENT ON COLUMN pl.zpcd IS 'Postcode';
CREATE SEQUENCE pm_bookmark_seq
    START WITH 1333065489701667072
    INCREMENT BY 1
    MINVALUE 1333065489701667072
    MAXVALUE 1369094286720630783
    CACHE 1;
ALTER TABLE pm_bookmark_seq OWNER TO ncuser;
CREATE TABLE pm (
    _bookmark_ bigint DEFAULT nextval('pm_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    pmid character varying,
    crus character varying,
    crdt date,
    crtm character varying,
    stat character varying(1),
    vsno integer,
    clid character varying,
    pmcd character varying,
    rcdt date,
    rctm character varying,
    amnt numeric(10,2),
    amn2 numeric(10,2),
    aamn numeric(10,2),
    aam2 numeric(10,2),
    ualc numeric(10,2),
    ual2 numeric(10,2),
    clos character varying(1),
    rlsd character varying(1),
    type character varying
);
ALTER TABLE pm OWNER TO ncuser;
COMMENT ON TABLE pm IS 'Payment';
COMMENT ON COLUMN pm.crcd IS 'Courier Code';
COMMENT ON COLUMN pm.pmid IS 'Unique id for PM table';
COMMENT ON COLUMN pm.crus IS 'User created';
COMMENT ON COLUMN pm.crdt IS 'Date of record';
COMMENT ON COLUMN pm.crtm IS 'Time of record';
COMMENT ON COLUMN pm.stat IS 'Status';
COMMENT ON COLUMN pm.vsno IS 'Version no';
COMMENT ON COLUMN pm.clid IS 'Client Id';
COMMENT ON COLUMN pm.pmcd IS 'Reference ';
COMMENT ON COLUMN pm.rcdt IS 'Payment receipt date';
COMMENT ON COLUMN pm.rctm IS 'Payment receipt time';
COMMENT ON COLUMN pm.amnt IS 'Receipt amount in courier currency';
COMMENT ON COLUMN pm.amn2 IS 'Receipt amount in client currency';
COMMENT ON COLUMN pm.aamn IS 'Allocated amount in courier currency';
COMMENT ON COLUMN pm.aam2 IS 'Allocated amount in client currency';
COMMENT ON COLUMN pm.ualc IS 'Unallocated amount in courier currency';
COMMENT ON COLUMN pm.ual2 IS 'Unallocated amount in client currency';
COMMENT ON COLUMN pm.clos IS 'Payment close';
COMMENT ON COLUMN pm.rlsd IS 'Payment realized or not';
COMMENT ON COLUMN pm.type IS 'Payment type';
CREATE SEQUENCE po_bookmark_seq
    START WITH 1621295865853378816
    INCREMENT BY 1
    MINVALUE 1621295865853378816
    MAXVALUE 1657324662872342527
    CACHE 1;
ALTER TABLE po_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE pp_bookmark_seq
    START WITH 3062447746611937536
    INCREMENT BY 1
    MINVALUE 3062447746611937536
    MAXVALUE 3098476543630901247
    CACHE 1;
ALTER TABLE pp_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE pr_bookmark_seq
    START WITH 1693353459891306752
    INCREMENT BY 1
    MINVALUE 1693353459891306752
    MAXVALUE 1729382256910270463
    CACHE 1;
ALTER TABLE pr_bookmark_seq OWNER TO ncuser;
CREATE TABLE pr (
    _bookmark_ bigint DEFAULT nextval('pr_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    pcid character varying,
    stwt numeric(10,2),
    bswt numeric(10,2),
    bspr numeric(11,2),
    exwt numeric(8,2),
    expr numeric(11,2),
    stat character varying(1),
    type character varying(1),
    clpr numeric(10,2),
    suid character varying,
    enwt numeric(10,2),
    prid character varying,
    tbuf numeric(10,2),
    tbut numeric(10,2),
    chfl character varying,
    chml character varying,
    chpc character varying(1),
    bsut character varying,
    buml character varying,
    vsno integer
);
ALTER TABLE pr OWNER TO ncuser;
COMMENT ON TABLE pr IS 'Price line file';
COMMENT ON COLUMN pr.crcd IS 'Courier Unique Code';
COMMENT ON COLUMN pr.pcid IS 'Price Chart Id';
COMMENT ON COLUMN pr.stwt IS 'Start Weight';
COMMENT ON COLUMN pr.bswt IS 'Base Weight';
COMMENT ON COLUMN pr.bspr IS 'Base Price';
COMMENT ON COLUMN pr.exwt IS 'Extra Weight';
COMMENT ON COLUMN pr.expr IS 'Extra Price';
COMMENT ON COLUMN pr.stat IS 'Record Status';
COMMENT ON COLUMN pr.type IS 'Type of Price Chart';
COMMENT ON COLUMN pr.clpr IS 'Calculated Price';
COMMENT ON COLUMN pr.suid IS 'Supplier Id';
COMMENT ON COLUMN pr.enwt IS 'End Weight';
COMMENT ON COLUMN pr.prid IS 'Price line unique id';
COMMENT ON COLUMN pr.tbuf IS 'Table band unit from';
COMMENT ON COLUMN pr.tbut IS 'Table band unit to';
COMMENT ON COLUMN pr.chfl IS 'Charge by field';
COMMENT ON COLUMN pr.chml IS 'Charge by multiplier';
COMMENT ON COLUMN pr.chpc IS 'Charge by percent flag';
COMMENT ON COLUMN pr.bsut IS 'Weight Unit';
COMMENT ON COLUMN pr.buml IS 'Base unit multiplier';
COMMENT ON COLUMN pr.vsno IS 'Version No.';
CREATE SEQUENCE proc_bookmark_seq
    START WITH 396316767208603648
    INCREMENT BY 1
    MINVALUE 396316767208603648
    MAXVALUE 432345564227567615
    CACHE 1;
ALTER TABLE proc_bookmark_seq OWNER TO ncuser;
CREATE TABLE proc (
    _bookmark_ bigint DEFAULT nextval('proc_bookmark_seq'::regclass) NOT NULL,
    name character varying,
    ptyp character varying(1),
    title character varying,
    srcf character varying,
    comf character varying,
    authn character varying,
    crdt date,
    evsno integer,
    pvsno character varying,
    amhis character varying,
    two character varying,
    three character varying,
    srcl character varying,
    objl character varying,
    amdt date,
    amtm character varying,
    cmdt date,
    cmtm character varying,
    inuse character varying
);
ALTER TABLE proc OWNER TO ncuser;
COMMENT ON TABLE proc IS 'System File 10 - Procedure details';
COMMENT ON COLUMN proc.name IS '[11.1] Procedure name';
COMMENT ON COLUMN proc.ptyp IS '[11.2] Procedure type';
COMMENT ON COLUMN proc.title IS '[11.3] Description';
COMMENT ON COLUMN proc.srcf IS '[11.4] Source file name';
COMMENT ON COLUMN proc.comf IS '[11.5] Compiled file name';
COMMENT ON COLUMN proc.authn IS '[11.6] Authors name';
COMMENT ON COLUMN proc.crdt IS '[11.7] Date created';
COMMENT ON COLUMN proc.evsno IS '[11.8] Edit version no.';
COMMENT ON COLUMN proc.pvsno IS '[11.9] Procedure version no.';
COMMENT ON COLUMN proc.amhis IS '[11.10] Amendment history 1';
COMMENT ON COLUMN proc.two IS '[11.11] 2';
COMMENT ON COLUMN proc.three IS '[11.12] 3';
COMMENT ON COLUMN proc.srcl IS '[11.15] Source file location';
COMMENT ON COLUMN proc.objl IS '[11.16] Object file location';
COMMENT ON COLUMN proc.amdt IS '[11.17] Date last amended';
COMMENT ON COLUMN proc.amtm IS '[11.18] Time last amended';
COMMENT ON COLUMN proc.cmdt IS '[11.19] Date last compiled';
COMMENT ON COLUMN proc.cmtm IS '[11.20] Time last compiled';
COMMENT ON COLUMN proc.inuse IS '[11.21] In use flag';
CREATE SEQUENCE ps_bookmark_seq
    START WITH 2774217370460225792
    INCREMENT BY 1
    MINVALUE 2774217370460225792
    MAXVALUE 2810246167479189503
    CACHE 1;
ALTER TABLE ps_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE pt_bookmark_seq
    START WITH 2305843009213694208
    INCREMENT BY 1
    MINVALUE 2305843009213694208
    MAXVALUE 2341871806232657919
    CACHE 1;
ALTER TABLE pt_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE pu_bookmark_seq
    START WITH 3422735716801577216
    INCREMENT BY 1
    MINVALUE 3422735716801577216
    MAXVALUE 3458764513820540927
    CACHE 1;
ALTER TABLE pu_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE py_bookmark_seq
    START WITH 3458764513820541184
    INCREMENT BY 1
    MINVALUE 3458764513820541184
    MAXVALUE 3494793310839504895
    CACHE 1;
ALTER TABLE py_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE qe_bookmark_seq
    START WITH 3927138875067072768
    INCREMENT BY 1
    MINVALUE 3927138875067072768
    MAXVALUE 3963167672086036479
    CACHE 1;
ALTER TABLE qe_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE rd_bookmark_seq
    START WITH 1765411053929234688
    INCREMENT BY 1
    MINVALUE 1765411053929234688
    MAXVALUE 1801439850948198399
    CACHE 1;
ALTER TABLE rd_bookmark_seq OWNER TO ncuser;
CREATE TABLE rd (
    _bookmark_ bigint DEFAULT nextval('rd_bookmark_seq'::regclass) NOT NULL,
    "FROM" character varying,
    "TO" character varying,
    key1 character varying,
    fld2 character varying,
    key3 character varying,
    fld3 character varying,
    stat character varying(1),
    othr character varying,
    fld1 character varying,
    key2 character varying,
    "DESC" character varying
);
ALTER TABLE rd OWNER TO ncuser;
COMMENT ON TABLE rd IS 'Relational Table';
COMMENT ON COLUMN rd."FROM" IS 'Base Field';
COMMENT ON COLUMN rd."TO" IS 'Linked File';
COMMENT ON COLUMN rd.key1 IS 'Key 1';
COMMENT ON COLUMN rd.fld2 IS 'Field 2';
COMMENT ON COLUMN rd.key3 IS 'Key 3';
COMMENT ON COLUMN rd.fld3 IS 'Field 3';
COMMENT ON COLUMN rd.stat IS 'Record Status';
COMMENT ON COLUMN rd.othr IS 'Others Field';
COMMENT ON COLUMN rd.fld1 IS 'Field 1';
COMMENT ON COLUMN rd.key2 IS 'Key 2';
COMMENT ON COLUMN rd."DESC" IS 'Description';
CREATE SEQUENCE re_bookmark_seq
    START WITH 3819052484010180864
    INCREMENT BY 1
    MINVALUE 3819052484010180864
    MAXVALUE 3855081281029144575
    CACHE 1;
ALTER TABLE re_bookmark_seq OWNER TO ncuser;
CREATE TABLE re (
    _bookmark_ bigint DEFAULT nextval('re_bookmark_seq'::regclass) NOT NULL,
    reid character varying,
    crcd character varying,
    rtid character varying,
    exid character varying,
    stat character varying(1),
    sequ integer,
    vsno integer
);
ALTER TABLE re OWNER TO ncuser;
COMMENT ON TABLE re IS 'Route extras';
COMMENT ON COLUMN re.reid IS 'Unique id';
COMMENT ON COLUMN re.crcd IS 'Courier Code';
COMMENT ON COLUMN re.rtid IS 'Route id';
COMMENT ON COLUMN re.exid IS 'Field 2';
COMMENT ON COLUMN re.stat IS 'Record Status';
COMMENT ON COLUMN re.sequ IS 'Sequence';
CREATE TABLE rec (
    failed bigint,
    success bigint
);
ALTER TABLE rec OWNER TO ncuser;
CREATE SEQUENCE reln_bookmark_seq
    START WITH 324259173170675712
    INCREMENT BY 1
    MINVALUE 324259173170675712
    MAXVALUE 360287970189639679
    CACHE 1;
ALTER TABLE reln_bookmark_seq OWNER TO ncuser;
CREATE TABLE reln (
    _bookmark_ bigint DEFAULT nextval('reln_bookmark_seq'::regclass) NOT NULL,
    filfrm character varying,
    filto character varying,
    title character varying,
    kname character varying,
    fld1 character varying,
    fld2 character varying,
    fld3 character varying,
    fld4 character varying,
    fld5 character varying,
    fld6 character varying,
    fld7 character varying,
    fld8 character varying,
    fld9 character varying,
    fld10 character varying
);
ALTER TABLE reln OWNER TO ncuser;
COMMENT ON TABLE reln IS 'System File 9 - Relations';
COMMENT ON COLUMN reln.filfrm IS '[9.1] File from';
COMMENT ON COLUMN reln.filto IS '[9.2] File to';
COMMENT ON COLUMN reln.title IS '[9.3] Description';
COMMENT ON COLUMN reln.kname IS '[9.4] Key name';
COMMENT ON COLUMN reln.fld1 IS '[9.5] Field 1';
COMMENT ON COLUMN reln.fld2 IS '[9.6] Field 2';
COMMENT ON COLUMN reln.fld3 IS '[9.7] Field 3';
COMMENT ON COLUMN reln.fld4 IS '[9.8] Field 4';
COMMENT ON COLUMN reln.fld5 IS '[9.9] Field 5';
COMMENT ON COLUMN reln.fld6 IS '[9.10] Field 6';
COMMENT ON COLUMN reln.fld7 IS '[9.11] Field 7';
COMMENT ON COLUMN reln.fld8 IS '[9.12] Field 8';
COMMENT ON COLUMN reln.fld9 IS '[9.13] Field 9';
COMMENT ON COLUMN reln.fld10 IS '[9.14] Field 10';
CREATE SEQUENCE report_serial_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE report_serial_seq OWNER TO ncuser;
CREATE SEQUENCE rh_bookmark_seq
    START WITH 4107282860161892608
    INCREMENT BY 1
    MINVALUE 4107282860161892608
    MAXVALUE 4143311657180856319
    CACHE 1;
ALTER TABLE rh_bookmark_seq OWNER TO ncuser;
CREATE TABLE rh (
    _bookmark_ bigint DEFAULT nextval('rh_bookmark_seq'::regclass) NOT NULL,
    rhcd bigint,
    "DESC" character varying,
    stat character varying,
    delf character varying(1),
    type character varying,
    rhdt date,
    uscd character varying,
    rhtm character varying,
    comm character varying,
    mf01 character varying,
    mf02 character varying,
    mf03 character varying,
    mf04 character varying,
    mf05 character varying
);
ALTER TABLE rh OWNER TO ncuser;
COMMENT ON TABLE rh IS 'Report history';
COMMENT ON COLUMN rh.rhcd IS 'Report code';
COMMENT ON COLUMN rh."DESC" IS 'Report name';
COMMENT ON COLUMN rh.stat IS 'Status';
COMMENT ON COLUMN rh.delf IS 'Deleted flag';
COMMENT ON COLUMN rh.type IS 'Report type';
COMMENT ON COLUMN rh.rhdt IS 'Run at date';
COMMENT ON COLUMN rh.uscd IS 'Run by user';
COMMENT ON COLUMN rh.rhtm IS 'Run at time';
COMMENT ON COLUMN rh.comm IS 'Comment / explanation';
COMMENT ON COLUMN rh.mf01 IS 'Misc field 1';
COMMENT ON COLUMN rh.mf02 IS 'Misc field 2';
COMMENT ON COLUMN rh.mf03 IS 'Misc field 3';
COMMENT ON COLUMN rh.mf04 IS 'Misc field 4';
COMMENT ON COLUMN rh.mf05 IS 'Misc field 5';
CREATE SEQUENCE rl_bookmark_seq
    START WITH 3783023686991216896
    INCREMENT BY 1
    MINVALUE 3783023686991216896
    MAXVALUE 3819052484010180607
    CACHE 1;
ALTER TABLE rl_bookmark_seq OWNER TO ncuser;
CREATE TABLE rl (
    _bookmark_ bigint DEFAULT nextval('rl_bookmark_seq'::regclass) NOT NULL,
    rlid character varying,
    crcd character varying,
    rtid character varying,
    lgid character varying,
    stat character varying(1),
    sequ integer,
    pcid character varying,
    vsno integer
);
ALTER TABLE rl OWNER TO ncuser;
COMMENT ON TABLE rl IS 'Route legs';
COMMENT ON COLUMN rl.rlid IS 'Unique id';
COMMENT ON COLUMN rl.crcd IS 'Courier Code';
COMMENT ON COLUMN rl.rtid IS 'Route id';
COMMENT ON COLUMN rl.lgid IS 'Leg id';
COMMENT ON COLUMN rl.stat IS 'Record Status';
COMMENT ON COLUMN rl.sequ IS 'Sequence';
COMMENT ON COLUMN rl.pcid IS 'Price chart';
CREATE SEQUENCE rp_bookmark_seq
    START WITH 3710966092953288960
    INCREMENT BY 1
    MINVALUE 3710966092953288960
    MAXVALUE 3746994889972252671
    CACHE 1;
ALTER TABLE rp_bookmark_seq OWNER TO ncuser;
CREATE TABLE rp (
    _bookmark_ bigint DEFAULT nextval('rp_bookmark_seq'::regclass) NOT NULL,
    rpcd character varying,
    "DESC" character varying,
    type character varying,
    scld character varying,
    emad character varying,
    delf character varying(1),
    date date,
    "time" character varying,
    drun date,
    trun character varying,
    uscd character varying,
    pdfu character varying,
    csvu character varying,
    datu character varying,
    xmlu character varying,
    tmpl character varying,
    dele character varying(1),
    stpi character varying,
    pgls character varying,
    bocd character varying,
    wgcd character varying,
    chcd character varying,
    clcd character varying,
    cols character varying,
    ctls character varying,
    cucd character varying,
    dels character varying,
    expc character varying(1),
    depp character varying,
    depf date,
    dept date,
    sldp character varying,
    sldf date,
    sldt date,
    optp character varying,
    optf date,
    optt date,
    ordr character varying(1),
    pmls character varying,
    prls character varying,
    rels character varying,
    sgls character varying,
    suls character varying,
    tok1 character varying,
    tok2 character varying,
    synb character varying(1),
    bol1 character varying(1),
    bol2 character varying(1),
    bol3 character varying(1),
    bol4 character varying(1),
    bol5 character varying(1),
    sel1 character varying,
    sel2 character varying,
    sel3 character varying,
    dr1p character varying,
    dr1f date,
    dr1t date,
    dr2p character varying,
    dr2f date,
    dr2t date,
    dr3p character varying,
    dr3f date,
    dr3t date,
    rngf character varying,
    rngt character varying,
    f001 character varying,
    f002 character varying,
    f003 character varying,
    f004 character varying,
    f005 character varying,
    f006 character varying,
    f007 character varying,
    f008 character varying,
    f009 character varying,
    f010 character varying,
    f011 character varying,
    f012 character varying,
    f013 character varying,
    f014 character varying,
    f015 character varying,
    f016 character varying,
    f017 character varying,
    f018 character varying,
    f019 character varying,
    f020 character varying
);
ALTER TABLE rp OWNER TO ncuser;
COMMENT ON TABLE rp IS 'Report parameters';
COMMENT ON COLUMN rp.rpcd IS 'Report code';
COMMENT ON COLUMN rp."DESC" IS 'Report description';
COMMENT ON COLUMN rp.type IS 'Report type (id)';
COMMENT ON COLUMN rp.scld IS 'Report schedule information';
COMMENT ON COLUMN rp.emad IS 'Email addresses of report recipient';
COMMENT ON COLUMN rp.delf IS 'Deleted flag';
COMMENT ON COLUMN rp.date IS 'Last update date';
COMMENT ON COLUMN rp."time" IS 'Last update time';
COMMENT ON COLUMN rp.drun IS 'Last run date';
COMMENT ON COLUMN rp.trun IS 'Last run time';
COMMENT ON COLUMN rp.uscd IS 'User last accessing';
COMMENT ON COLUMN rp.pdfu IS 'PDF output file name';
COMMENT ON COLUMN rp.csvu IS 'CSV output file name';
COMMENT ON COLUMN rp.datu IS 'Original data output file name';
COMMENT ON COLUMN rp.xmlu IS 'XML output file name';
COMMENT ON COLUMN rp.tmpl IS 'Email body template';
COMMENT ON COLUMN rp.dele IS 'Level of detail in report';
COMMENT ON COLUMN rp.stpi IS 'Booking status pick code';
COMMENT ON COLUMN rp.pgls IS 'Programme list';
COMMENT ON COLUMN rp.bocd IS 'Booking number';
COMMENT ON COLUMN rp.wgcd IS 'Work group code';
COMMENT ON COLUMN rp.chcd IS 'Chain code';
COMMENT ON COLUMN rp.clcd IS 'Agent (client) code';
COMMENT ON COLUMN rp.cols IS 'Product list';
COMMENT ON COLUMN rp.ctls IS 'Product type list';
COMMENT ON COLUMN rp.cucd IS 'Currency';
COMMENT ON COLUMN rp.dels IS 'Destination list';
COMMENT ON COLUMN rp.expc IS 'Exclude private clients';
COMMENT ON COLUMN rp.depp IS 'Departure date period';
COMMENT ON COLUMN rp.depf IS 'Departure date from';
COMMENT ON COLUMN rp.dept IS 'Departure date to';
COMMENT ON COLUMN rp.sldp IS 'Sold date period';
COMMENT ON COLUMN rp.sldf IS 'Sold date from';
COMMENT ON COLUMN rp.sldt IS 'Sold date to';
COMMENT ON COLUMN rp.optp IS 'Optioned date period';
COMMENT ON COLUMN rp.optf IS 'Optioned date from';
COMMENT ON COLUMN rp.optt IS 'Optioned date to';
COMMENT ON COLUMN rp.ordr IS 'Order by';
COMMENT ON COLUMN rp.pmls IS 'Payment type list';
COMMENT ON COLUMN rp.prls IS 'Property list';
COMMENT ON COLUMN rp.rels IS 'Resort list';
COMMENT ON COLUMN rp.sgls IS 'Supplier group list';
COMMENT ON COLUMN rp.suls IS 'Supplier list';
COMMENT ON COLUMN rp.tok1 IS 'First subtotal pick code';
COMMENT ON COLUMN rp.tok2 IS 'Second subtotal pick code';
COMMENT ON COLUMN rp.synb IS 'Selection Yes No or Both';
COMMENT ON COLUMN rp.bol1 IS 'Boolean field 1';
COMMENT ON COLUMN rp.bol2 IS 'Boolean field 2';
COMMENT ON COLUMN rp.bol3 IS 'Boolean field 3';
COMMENT ON COLUMN rp.bol4 IS 'Boolean field 4';
COMMENT ON COLUMN rp.bol5 IS 'Boolean field 5';
COMMENT ON COLUMN rp.sel1 IS 'Selection (enum) field 1';
COMMENT ON COLUMN rp.sel2 IS 'Selection (enum) field 2';
COMMENT ON COLUMN rp.sel3 IS 'Selection (enum) field 3';
COMMENT ON COLUMN rp.dr1p IS 'Date range 1 period';
COMMENT ON COLUMN rp.dr1f IS 'Date range 1 from';
COMMENT ON COLUMN rp.dr1t IS 'Date range 1 to';
COMMENT ON COLUMN rp.dr2p IS 'Date range 2 period';
COMMENT ON COLUMN rp.dr2f IS 'Date range 2 from';
COMMENT ON COLUMN rp.dr2t IS 'Date range 2 to';
COMMENT ON COLUMN rp.dr3p IS 'Date range 3 period';
COMMENT ON COLUMN rp.dr3f IS 'Date range 3 from';
COMMENT ON COLUMN rp.dr3t IS 'Date range 3 to';
COMMENT ON COLUMN rp.rngf IS 'Data range from';
COMMENT ON COLUMN rp.rngt IS 'Data range to';
COMMENT ON COLUMN rp.f001 IS 'Report field 001';
COMMENT ON COLUMN rp.f002 IS 'Report field 002';
COMMENT ON COLUMN rp.f003 IS 'Report field 003';
COMMENT ON COLUMN rp.f004 IS 'Report field 004';
COMMENT ON COLUMN rp.f005 IS 'Report field 005';
COMMENT ON COLUMN rp.f006 IS 'Report field 006';
COMMENT ON COLUMN rp.f007 IS 'Report field 007';
COMMENT ON COLUMN rp.f008 IS 'Report field 008';
COMMENT ON COLUMN rp.f009 IS 'Report field 009';
COMMENT ON COLUMN rp.f010 IS 'Report field 010';
COMMENT ON COLUMN rp.f011 IS 'Report field 011';
COMMENT ON COLUMN rp.f012 IS 'Report field 012';
COMMENT ON COLUMN rp.f013 IS 'Report field 013';
COMMENT ON COLUMN rp.f014 IS 'Report field 014';
COMMENT ON COLUMN rp.f015 IS 'Report field 015';
COMMENT ON COLUMN rp.f016 IS 'Report field 016';
COMMENT ON COLUMN rp.f017 IS 'Report field 017';
COMMENT ON COLUMN rp.f018 IS 'Report field 018';
COMMENT ON COLUMN rp.f019 IS 'Report field 019';
COMMENT ON COLUMN rp.f020 IS 'Report field 020';
CREATE SEQUENCE rs_bookmark_seq
    START WITH 3026418949592973568
    INCREMENT BY 1
    MINVALUE 3026418949592973568
    MAXVALUE 3062447746611937279
    CACHE 1;
ALTER TABLE rs_bookmark_seq OWNER TO ncuser;
CREATE TABLE rs (
    _bookmark_ bigint DEFAULT nextval('rs_bookmark_seq'::regclass) NOT NULL,
    rscd character varying,
    "DESC" character varying,
    dat1 date,
    dat2 date,
    days integer,
    date date,
    delf character varying(1),
    mfdt date,
    mfus character varying,
    mftm character varying
);
ALTER TABLE rs OWNER TO ncuser;
COMMENT ON TABLE rs IS 'Release Styles';
COMMENT ON COLUMN rs.rscd IS 'Release style code';
COMMENT ON COLUMN rs."DESC" IS 'Release style description';
COMMENT ON COLUMN rs.dat1 IS 'Start date';
COMMENT ON COLUMN rs.dat2 IS 'End date';
COMMENT ON COLUMN rs.days IS 'Days to release';
COMMENT ON COLUMN rs.date IS 'Date of release';
COMMENT ON COLUMN rs.delf IS 'Delete flag';
COMMENT ON COLUMN rs.mfdt IS 'Date last modified';
COMMENT ON COLUMN rs.mfus IS 'Modified by user';
COMMENT ON COLUMN rs.mftm IS 'Modified at terminal';
CREATE SEQUENCE rt_bookmark_seq
    START WITH 2810246167479189760
    INCREMENT BY 1
    MINVALUE 2810246167479189760
    MAXVALUE 2846274964498153471
    CACHE 1;
ALTER TABLE rt_bookmark_seq OWNER TO ncuser;
CREATE TABLE rt (
    _bookmark_ bigint DEFAULT nextval('rt_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    rtid character varying,
    rtcd character varying,
    pdid character varying,
    fzid character varying,
    tzid character varying,
    ctyp character varying,
    slid character varying,
    fdat date,
    tdat date,
    note character varying,
    prio character varying,
    type character varying,
    stat character varying(1),
    vsno integer,
    adlg character varying(1),
    fpid character varying,
    suid character varying,
    "DESC" character varying,
    tcut character varying,
    labl character varying,
    tday character varying,
    etdp character varying,
    etav character varying,
    dpid character varying,
    acol character varying(1),
    ctet character varying,
    dtet character varying,
    dtype character varying(31),
    routeextras bytea,
    routelegs bytea
);
ALTER TABLE rt OWNER TO ncuser;
COMMENT ON TABLE rt IS 'Route File';
COMMENT ON COLUMN rt.crcd IS 'Courier Code';
COMMENT ON COLUMN rt.rtid IS 'Route ID';
COMMENT ON COLUMN rt.rtcd IS 'Route Code';
COMMENT ON COLUMN rt.pdid IS 'Product ID';
COMMENT ON COLUMN rt.fzid IS 'From zone id';
COMMENT ON COLUMN rt.tzid IS 'To zone id';
COMMENT ON COLUMN rt.ctyp IS 'Contents id';
COMMENT ON COLUMN rt.slid IS 'Service level id';
COMMENT ON COLUMN rt.fdat IS 'Valid from date';
COMMENT ON COLUMN rt.tdat IS 'Valid to date';
COMMENT ON COLUMN rt.note IS 'dispatch notes';
COMMENT ON COLUMN rt.prio IS 'Priority';
COMMENT ON COLUMN rt.type IS 'Route type';
COMMENT ON COLUMN rt.stat IS 'Record Status';
COMMENT ON COLUMN rt.vsno IS 'Version no';
COMMENT ON COLUMN rt.adlg IS 'Auto delivery leg';
COMMENT ON COLUMN rt.fpid IS 'From place';
COMMENT ON COLUMN rt.suid IS 'Supplier id';
COMMENT ON COLUMN rt."DESC" IS 'Route description';
COMMENT ON COLUMN rt.tcut IS 'Cut-off time';
COMMENT ON COLUMN rt.labl IS 'Label text';
COMMENT ON COLUMN rt.tday IS 'Transit days';
COMMENT ON COLUMN rt.etdp IS 'Departure time';
COMMENT ON COLUMN rt.etav IS 'Arrival time';
COMMENT ON COLUMN rt.dpid IS 'Departure port (place) id';
COMMENT ON COLUMN rt.acol IS 'Auto collection leg';
COMMENT ON COLUMN rt.ctet IS 'Collection time estimation text';
COMMENT ON COLUMN rt.dtet IS 'Delivery time estimation text';
CREATE SEQUENCE sa_bookmark_seq
    START WITH 2918332558536081664
    INCREMENT BY 1
    MINVALUE 2918332558536081664
    MAXVALUE 2954361355555045375
    CACHE 1;
ALTER TABLE sa_bookmark_seq OWNER TO ncuser;
CREATE TABLE sa (
    _bookmark_ bigint DEFAULT nextval('sa_bookmark_seq'::regclass) NOT NULL,
    sacd character varying,
    "DESC" character varying,
    delf character varying(1),
    prod character varying,
    prsq character varying,
    noml integer,
    comm numeric(4,2),
    prft numeric(4,2),
    repc integer,
    repr integer,
    mfdt date,
    mfus character varying,
    mftm character varying,
    incm numeric(4,2)
);
ALTER TABLE sa OWNER TO ncuser;
COMMENT ON TABLE sa IS 'Sales Analysis Code';
COMMENT ON COLUMN sa.sacd IS 'Sales Analysis Code';
COMMENT ON COLUMN sa."DESC" IS 'Description';
COMMENT ON COLUMN sa.delf IS 'Deleted flag';
COMMENT ON COLUMN sa.prod IS 'Product Type';
COMMENT ON COLUMN sa.prsq IS 'Product Sequence';
COMMENT ON COLUMN sa.noml IS 'Nominal Ledg link code';
COMMENT ON COLUMN sa.comm IS 'Commission Rate';
COMMENT ON COLUMN sa.prft IS 'Required Profit Margin';
COMMENT ON COLUMN sa.repc IS 'P.Book reporting Column';
COMMENT ON COLUMN sa.repr IS 'P.Book Reporting Row';
COMMENT ON COLUMN sa.mfdt IS 'Date last modified';
COMMENT ON COLUMN sa.mfus IS 'Modified by User';
COMMENT ON COLUMN sa.mftm IS 'Modified at terminal';
COMMENT ON COLUMN sa.incm IS 'Inside commission receivable';
CREATE TABLE sales (
    cv_spne numeric,
    sp01 numeric,
    sp02 numeric,
    sp03 numeric,
    sp04 numeric,
    sp05 numeric,
    sp06 numeric,
    sp07 numeric,
    sp08 numeric,
    sp09 numeric,
    sp10 numeric,
    sp11 numeric,
    sp12 numeric,
    sp13 numeric,
    sp14 numeric,
    sp15 numeric,
    sp16 numeric,
    sp17 numeric,
    sp18 numeric,
    sp19 numeric,
    sp20 numeric
);
ALTER TABLE sales OWNER TO ncuser;
CREATE SEQUENCE sc_bookmark_seq
    START WITH 1693353459891306752
    INCREMENT BY 1
    MINVALUE 1693353459891306752
    MAXVALUE 1729382256910270463
    CACHE 1;
ALTER TABLE sc_bookmark_seq OWNER TO ncuser;
CREATE TABLE sc (
    _bookmark_ bigint DEFAULT nextval('sc_bookmark_seq'::regclass) NOT NULL,
    sccd character varying,
    "DESC" character varying,
    delf character varying(1)
);
ALTER TABLE sc OWNER TO ncuser;
COMMENT ON TABLE sc IS 'Source codes';
COMMENT ON COLUMN sc.sccd IS 'Source code';
COMMENT ON COLUMN sc."DESC" IS 'Text';
COMMENT ON COLUMN sc.delf IS 'Deleted flag';
CREATE SEQUENCE se_bookmark_seq
    START WITH 1945555039024054528
    INCREMENT BY 1
    MINVALUE 1945555039024054528
    MAXVALUE 1981583836043018239
    CACHE 1;
ALTER TABLE se_bookmark_seq OWNER TO ncuser;
CREATE TABLE se (
    _bookmark_ bigint DEFAULT nextval('se_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    secd character varying,
    usid character varying,
    date date,
    "time" character varying,
    lupd character varying,
    stat character varying(1),
    vsno integer DEFAULT 1,
    seid character varying,
    sess character varying,
    txt1 character varying,
    txt2 character varying,
    clid character varying,
    "CTID" character varying,
    bkcl character varying,
    ipad character varying,
    tokn character varying,
    hmac character varying
);
ALTER TABLE se OWNER TO ncuser;
COMMENT ON TABLE se IS 'Session File';
COMMENT ON COLUMN se.crcd IS 'Courier Code';
COMMENT ON COLUMN se.secd IS 'Session Code';
COMMENT ON COLUMN se.usid IS 'User Id';
COMMENT ON COLUMN se.date IS 'Date Started';
COMMENT ON COLUMN se."time" IS 'Time Started';
COMMENT ON COLUMN se.lupd IS 'Last Updated time';
COMMENT ON COLUMN se.stat IS 'Record Status';
COMMENT ON COLUMN se.vsno IS 'Version No.';
COMMENT ON COLUMN se.seid IS 'Session Id';
COMMENT ON COLUMN se.sess IS 'Session Code';
COMMENT ON COLUMN se.txt1 IS 'Session data 1';
COMMENT ON COLUMN se.txt2 IS 'Session data 2';
COMMENT ON COLUMN se.clid IS 'Login client Id';
COMMENT ON COLUMN se."CTID" IS 'Contact Id';
COMMENT ON COLUMN se.bkcl IS 'Booking client';
CREATE TABLE search (
    id bigint NOT NULL,
    recordtype character varying,
    recordid character varying,
    recordname character varying,
    search_phrase tsvector
);
ALTER TABLE search OWNER TO ncuser;
CREATE SEQUENCE search_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE search_id_seq OWNER TO ncuser;
ALTER SEQUENCE search_id_seq OWNED BY search.id;
CREATE SEQUENCE seq_gen_sequence
    START WITH 50
    INCREMENT BY 50
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER TABLE seq_gen_sequence OWNER TO ncuser;
CREATE TABLE sequence (
    seq_name character varying(50) NOT NULL,
    seq_count numeric(38,0)
);
ALTER TABLE sequence OWNER TO ncuser;
CREATE SEQUENCE si_bookmark_seq
    START WITH 1657324662872342784
    INCREMENT BY 1
    MINVALUE 1657324662872342784
    MAXVALUE 1693353459891306495
    CACHE 1;
ALTER TABLE si_bookmark_seq OWNER TO ncuser;
CREATE TABLE si (
    _bookmark_ bigint DEFAULT nextval('si_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    siid character varying,
    clid character varying,
    indt date,
    invn character varying,
    "DESC" character varying,
    neta numeric(18,2),
    vata numeric(18,2),
    tota numeric(18,2),
    stat character varying(1),
    vsno integer,
    type character varying(1),
    tnam character varying,
    crus character varying,
    crdt date,
    crtm character varying,
    path character varying,
    cuid character varying,
    cui2 character varying,
    net2 numeric(18,2),
    vat2 numeric(18,2),
    tot2 numeric(18,2),
    erat numeric(13,4),
    expt character varying(1),
    epth character varying,
    aamn numeric(18,2),
    aam2 numeric(18,2),
    ualc numeric(18,2),
    ual2 numeric(18,2),
    clos character varying(1),
    exto character varying,
    expn character varying
);
ALTER TABLE si OWNER TO ncuser;
COMMENT ON TABLE si IS 'Sales Invoice File';
COMMENT ON COLUMN si.crcd IS 'Courier Code';
COMMENT ON COLUMN si.siid IS 'Sales invoice';
COMMENT ON COLUMN si.clid IS 'Client id';
COMMENT ON COLUMN si.indt IS 'Invoice Date';
COMMENT ON COLUMN si.invn IS 'Invoice No';
COMMENT ON COLUMN si."DESC" IS 'Invoice Description';
COMMENT ON COLUMN si.neta IS 'Net amount';
COMMENT ON COLUMN si.vata IS 'Vat amount';
COMMENT ON COLUMN si.tota IS 'Total amount';
COMMENT ON COLUMN si.stat IS 'Status';
COMMENT ON COLUMN si.vsno IS 'Version No.';
COMMENT ON COLUMN si.type IS 'Invoice Type';
COMMENT ON COLUMN si.tnam IS 'Invoice Type text';
COMMENT ON COLUMN si.crus IS 'Created user';
COMMENT ON COLUMN si.crdt IS 'Creation date';
COMMENT ON COLUMN si.crtm IS 'Creation time';
COMMENT ON COLUMN si.path IS 'Relative path';
COMMENT ON COLUMN si.cuid IS 'Currency 1';
COMMENT ON COLUMN si.cui2 IS '2nd currency';
COMMENT ON COLUMN si.net2 IS 'Net in 2nd currency';
COMMENT ON COLUMN si.vat2 IS 'VAT in 2nd currency';
COMMENT ON COLUMN si.tot2 IS 'Total in 2nd currency';
COMMENT ON COLUMN si.erat IS 'Exchange rate';
COMMENT ON COLUMN si.expt IS 'Exported to accounts system';
COMMENT ON COLUMN si.epth IS 'Email path';
COMMENT ON COLUMN si.aamn IS 'Allocated amount in courier currency';
COMMENT ON COLUMN si.aam2 IS 'Allocated amount for the invoice in client currency';
COMMENT ON COLUMN si.ualc IS 'Unallocated amount in courier currency';
COMMENT ON COLUMN si.ual2 IS 'Unallocated amount in client currency';
COMMENT ON COLUMN si.clos IS 'Invoice closed flag';
COMMENT ON COLUMN si.exto IS 'Exported to';
COMMENT ON COLUMN si.expn IS 'Exporter program name';
CREATE SEQUENCE sm_bookmark_seq
    START WITH 1297036692682703104
    INCREMENT BY 1
    MINVALUE 1297036692682703104
    MAXVALUE 1333065489701666815
    CACHE 1;
ALTER TABLE sm_bookmark_seq OWNER TO ncuser;
CREATE TABLE sm (
    _bookmark_ bigint DEFAULT nextval('sm_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    smid character varying,
    smri character varying,
    itid character varying,
    flid character varying,
    tlid character varying,
    csid character varying,
    sqty integer,
    date date,
    "time" character varying,
    usid character varying,
    note character varying,
    stat character varying(1),
    vsno integer,
    dldt date,
    dltm character varying,
    dusr character varying,
    flsd character varying,
    tlsd character varying,
    iact character varying(1)
);
ALTER TABLE sm OWNER TO ncuser;
COMMENT ON TABLE sm IS 'Stock movement';
COMMENT ON COLUMN sm.crcd IS 'Courier Code';
COMMENT ON COLUMN sm.smid IS 'Stock movement id';
COMMENT ON COLUMN sm.smri IS 'Stock movement reverse id';
COMMENT ON COLUMN sm.itid IS 'Item id';
COMMENT ON COLUMN sm.flid IS 'From location id';
COMMENT ON COLUMN sm.tlid IS 'To location id';
COMMENT ON COLUMN sm.csid IS 'Consignment/job id';
COMMENT ON COLUMN sm.sqty IS 'Stock quantity';
COMMENT ON COLUMN sm.date IS 'Date';
COMMENT ON COLUMN sm."time" IS 'Time';
COMMENT ON COLUMN sm.usid IS 'User id';
COMMENT ON COLUMN sm.note IS 'Comment/Note';
COMMENT ON COLUMN sm.stat IS 'Status';
COMMENT ON COLUMN sm.vsno IS 'Version No.';
COMMENT ON COLUMN sm.dldt IS 'Deletion date';
COMMENT ON COLUMN sm.dltm IS 'Deletion time';
COMMENT ON COLUMN sm.dusr IS 'Deleted user id';
COMMENT ON COLUMN sm.flsd IS 'From location id for saved job';
COMMENT ON COLUMN sm.tlsd IS 'To location id for saved job';
COMMENT ON COLUMN sm.iact IS 'Inctive';
CREATE SEQUENCE sq_bookmark_seq
    START WITH 1261007895663739136
    INCREMENT BY 1
    MINVALUE 1261007895663739136
    MAXVALUE 1297036692682702847
    CACHE 1;
ALTER TABLE sq_bookmark_seq OWNER TO ncuser;
CREATE TABLE sq (
    _bookmark_ bigint DEFAULT nextval('sq_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    sqid character varying,
    itid character varying,
    loid character varying,
    sqty integer,
    stat character varying,
    vsno integer
);
ALTER TABLE sq OWNER TO ncuser;
COMMENT ON TABLE sq IS 'Stock quantity';
COMMENT ON COLUMN sq.crcd IS 'Courier Code';
COMMENT ON COLUMN sq.sqid IS 'Stock unique id';
COMMENT ON COLUMN sq.itid IS 'Item Id';
COMMENT ON COLUMN sq.loid IS 'Location id';
COMMENT ON COLUMN sq.sqty IS 'Stock quantity';
CREATE SEQUENCE su_bookmark_seq
    START WITH 2089670227099910400
    INCREMENT BY 1
    MINVALUE 2089670227099910400
    MAXVALUE 2125699024118874111
    CACHE 1;
ALTER TABLE su_bookmark_seq OWNER TO ncuser;
CREATE TABLE su (
    _bookmark_ bigint DEFAULT nextval('su_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    sucd character varying,
    type character varying(1),
    "DESC" character varying,
    note character varying,
    arid character varying,
    cuid character varying,
    bsut character varying(3),
    crmd character varying(1),
    stat character varying(1),
    vsno integer,
    suid character varying,
    oper character varying(1),
    grpc character varying(1),
    griv character varying(1),
    pcsu character varying,
    insu character varying,
    prio character varying,
    oman character varying(1),
    adch character varying(1),
    peml character varying,
    txt1 character varying,
    txt2 character varying,
    txt3 character varying,
    ades character varying,
    spec character varying,
    ana1 character varying,
    pcem character varying,
    pocf character varying(1),
    fsmp character varying(1),
    mtmp character varying,
    aocl numeric(11,2),
    aocp numeric(5,2),
    lpcd date,
    lpct character varying,
    bicu character varying,
    sicu character varying,
    surl character varying,
    adid character varying,
    dcut character varying(3),
    act1 character varying,
    act2 character varying,
    aps1 character varying,
    aps2 character varying,
    url1 character varying,
    url2 character varying,
    url3 character varying,
    url4 character varying,
    url5 character varying,
    url6 character varying,
    url7 character varying,
    url8 character varying,
    purl character varying,
    wbpl character varying(1),
    hcvl numeric(11,2),
    sid1 character varying,
    sid2 character varying,
    dmru character varying,
    tags character varying,
    mfcl character varying(1),
    demt character varying(1),
    debk character varying,
    dtpl character varying,
    dcty character varying,
    mctm character varying,
    trtm numeric(40,0),
    autp character varying(1),
    days character varying,
    apem character varying,
    ptmp character varying,
    apfx character varying,
    ccod character varying,
    iata character varying,
    nase character varying(1),
    sifm character varying,
    psmf character varying(1),
    etde character varying
);
ALTER TABLE su OWNER TO ncuser;
COMMENT ON TABLE su IS 'Supplier File';
COMMENT ON COLUMN su.crcd IS 'Courier Code';
COMMENT ON COLUMN su.sucd IS 'Supplier code';
COMMENT ON COLUMN su.type IS 'Supplier Type';
COMMENT ON COLUMN su."DESC" IS 'Supplier Name';
COMMENT ON COLUMN su.note IS 'Note';
COMMENT ON COLUMN su.arid IS 'Airport Code (for Agent Only)';
COMMENT ON COLUMN su.cuid IS 'Currency Code';
COMMENT ON COLUMN su.bsut IS 'Default Price Base Unit';
COMMENT ON COLUMN su.crmd IS 'Credit Request mode';
COMMENT ON COLUMN su.stat IS 'Record Status';
COMMENT ON COLUMN su.vsno IS 'Version No.';
COMMENT ON COLUMN su.suid IS 'Supplier Id';
COMMENT ON COLUMN su.oper IS 'Operational';
COMMENT ON COLUMN su.grpc IS 'Group Price Chart Supplier';
COMMENT ON COLUMN su.griv IS 'Group Invoice Suppliers';
COMMENT ON COLUMN su.pcsu IS 'Price Chart Supplier';
COMMENT ON COLUMN su.insu IS 'Invoice to Supplier';
COMMENT ON COLUMN su.prio IS 'Priority';
COMMENT ON COLUMN su.oman IS 'Automatically open manifest';
COMMENT ON COLUMN su.adch IS 'Accounting details changed';
COMMENT ON COLUMN su.peml IS 'Pre-alert manifest e-mail';
COMMENT ON COLUMN su.txt1 IS 'Additional text 1';
COMMENT ON COLUMN su.txt2 IS 'Additional Text 2';
COMMENT ON COLUMN su.txt3 IS 'Additional Text 3';
COMMENT ON COLUMN su.ades IS 'Additional Description';
COMMENT ON COLUMN su.spec IS 'Countries Specialized';
COMMENT ON COLUMN su.ana1 IS 'Analysis 1';
COMMENT ON COLUMN su.pcem IS 'POD Chase E-mail list';
COMMENT ON COLUMN su.pocf IS 'PoD Chase Frequency';
COMMENT ON COLUMN su.fsmp IS 'Print Manifest(Full/Shortform)';
COMMENT ON COLUMN su.mtmp IS 'Manifest Template';
COMMENT ON COLUMN su.aocl IS 'Inv. Reco. Over Charge Limit';
COMMENT ON COLUMN su.aocp IS 'Inv. Reco. Over Charge %';
COMMENT ON COLUMN su.lpcd IS 'Last PoD Chase Date';
COMMENT ON COLUMN su.lpct IS 'Last PoD Chase Time';
COMMENT ON COLUMN su.bicu IS 'Big Icon URL';
COMMENT ON COLUMN su.sicu IS 'Small Icon URL';
COMMENT ON COLUMN su.surl IS '3rd Party Track URL';
COMMENT ON COLUMN su.adid IS 'Supplier address';
COMMENT ON COLUMN su.dcut IS 'Distance unit';
COMMENT ON COLUMN su.act1 IS 'Access Account number 1';
COMMENT ON COLUMN su.act2 IS 'Access Account number 2';
COMMENT ON COLUMN su.aps1 IS 'Access pass 1';
COMMENT ON COLUMN su.aps2 IS 'Access pass 2';
COMMENT ON COLUMN su.url1 IS 'Web service URL 1';
COMMENT ON COLUMN su.url2 IS 'Web service URL 2';
COMMENT ON COLUMN su.url3 IS 'Web service URL 3';
COMMENT ON COLUMN su.url4 IS 'Web service URL 4';
COMMENT ON COLUMN su.url5 IS 'Web service URL 5';
COMMENT ON COLUMN su.url6 IS 'Web service URL 6';
COMMENT ON COLUMN su.url7 IS 'Web service URL 7';
COMMENT ON COLUMN su.url8 IS 'Web service URL 8';
COMMENT ON COLUMN su.purl IS 'Plugins URL';
COMMENT ON COLUMN su.wbpl IS 'Web booking plugin exist';
COMMENT ON COLUMN su.hcvl IS 'Harmonized code ceiling';
COMMENT ON COLUMN su.sid1 IS 'Site id 1';
COMMENT ON COLUMN su.sid2 IS 'Site id 2';
COMMENT ON COLUMN su.dmru IS 'Daily manifest report URL';
COMMENT ON COLUMN su.tags IS 'Tags';
COMMENT ON COLUMN su.mfcl IS 'Manifest close method';
COMMENT ON COLUMN su.demt IS 'Data exchange method';
COMMENT ON COLUMN su.debk IS 'Data exchange bookings';
COMMENT ON COLUMN su.dtpl IS 'Data exchange template';
COMMENT ON COLUMN su.dcty IS 'Date exchange contents type';
COMMENT ON COLUMN su.mctm IS 'Manifest close time';
COMMENT ON COLUMN su.trtm IS 'Data transfer every minutes';
COMMENT ON COLUMN su.autp IS 'Auto PoD Report enabled';
COMMENT ON COLUMN su.days IS 'Auto PoD enable for days';
COMMENT ON COLUMN su.apem IS 'Auto PoD email';
COMMENT ON COLUMN su.ptmp IS 'Auto PoD template';
COMMENT ON COLUMN su.apfx IS 'Airline prefix';
COMMENT ON COLUMN su.ccod IS 'Importing Carrier code';
COMMENT ON COLUMN su.iata IS 'Airline IATA code';
COMMENT ON COLUMN su.nase IS 'NetCourier Agent System enabled?';
COMMENT ON COLUMN su.sifm IS 'Supplier invoice format';
COMMENT ON COLUMN su.psmf IS 'Allow to print vendor manifest';
CREATE SEQUENCE sy_bookmark_seq
    START WITH 2161727821137838336
    INCREMENT BY 1
    MINVALUE 2161727821137838336
    MAXVALUE 2197756618156802047
    CACHE 1;
ALTER TABLE sy_bookmark_seq OWNER TO ncuser;
CREATE TABLE sy (
    _bookmark_ bigint DEFAULT nextval('sy_bookmark_seq'::regclass) NOT NULL,
    code character varying,
    dcss character varying,
    susr character varying,
    stat character varying(1),
    spas character varying,
    syid character varying,
    sytz character varying,
    lswd integer,
    lshg integer,
    lslf integer,
    lstp integer,
    mswd integer,
    mshg integer,
    mslf integer,
    mstp integer,
    sswd integer,
    sshg integer,
    sslf integer,
    sstp integer,
    crcd character varying,
    vsno integer
);
ALTER TABLE sy OWNER TO ncuser;
COMMENT ON TABLE sy IS 'System File';
COMMENT ON COLUMN sy.code IS 'System Code';
COMMENT ON COLUMN sy.dcss IS 'Default Style Sheet';
COMMENT ON COLUMN sy.susr IS 'Super User';
COMMENT ON COLUMN sy.stat IS 'Record Status';
COMMENT ON COLUMN sy.spas IS 'Super User''s Password';
COMMENT ON COLUMN sy.syid IS 'System Id';
COMMENT ON COLUMN sy.sytz IS 'System Time Zone';
COMMENT ON COLUMN sy.lswd IS 'Large screen width';
COMMENT ON COLUMN sy.lshg IS 'Large screen height';
COMMENT ON COLUMN sy.lslf IS 'Large screen left position';
COMMENT ON COLUMN sy.lstp IS 'Large screen top position';
COMMENT ON COLUMN sy.mswd IS 'Medium screen width';
COMMENT ON COLUMN sy.mshg IS 'Medium screen height';
COMMENT ON COLUMN sy.mslf IS 'Medium screen left position';
COMMENT ON COLUMN sy.mstp IS 'Medium screen top position';
COMMENT ON COLUMN sy.sswd IS 'Small screen width';
COMMENT ON COLUMN sy.sshg IS 'Small screen height';
COMMENT ON COLUMN sy.sslf IS 'Small screen left position';
COMMENT ON COLUMN sy.sstp IS 'Small screen top position';
CREATE SEQUENCE syst_bookmark_seq
    START WITH 36028797018964224
    INCREMENT BY 1
    MINVALUE 36028797018964224
    MAXVALUE 72057594037927935
    CACHE 1;
ALTER TABLE syst_bookmark_seq OWNER TO ncuser;
CREATE TABLE syst (
    _bookmark_ bigint DEFAULT nextval('syst_bookmark_seq'::regclass) NOT NULL,
    name character varying,
    title character varying,
    sloc character varying,
    grps character varying,
    vsno integer,
    lphysno integer,
    llogno integer,
    lkeyno integer,
    updsys character varying(1),
    datloc character varying,
    reploc character varying,
    prgloc character varying,
    objloc character varying,
    mnuprg character varying,
    mprgt character varying(1)
);
ALTER TABLE syst OWNER TO ncuser;
COMMENT ON TABLE syst IS 'System File 1 - System Details';
COMMENT ON COLUMN syst.name IS '[1.1] System name';
COMMENT ON COLUMN syst.title IS '[1.2] Description';
COMMENT ON COLUMN syst.sloc IS '[1.3] System Location';
COMMENT ON COLUMN syst.grps IS '[1.4] Groups';
COMMENT ON COLUMN syst.vsno IS '[1.5] Version no.';
COMMENT ON COLUMN syst.lphysno IS '[1.6] Last physical file no. allocated';
COMMENT ON COLUMN syst.llogno IS '[1.7] Last logical file no. allocated';
COMMENT ON COLUMN syst.lkeyno IS '[1.8] Last key no. allocated';
COMMENT ON COLUMN syst.updsys IS '[1.9] System needs UPDSYS (flag)';
COMMENT ON COLUMN syst.datloc IS '[1.10] Data files default location';
COMMENT ON COLUMN syst.reploc IS '[1.11] Report files default location';
COMMENT ON COLUMN syst.prgloc IS '[1.12] Program sources default location';
COMMENT ON COLUMN syst.objloc IS '[1.13] Program objects default location';
COMMENT ON COLUMN syst.mnuprg IS '[1.14] Menu program';
COMMENT ON COLUMN syst.mprgt IS '[1.15] Menu program type';
CREATE SEQUENCE ta_bookmark_seq
    START WITH 1945555039024054528
    INCREMENT BY 1
    MINVALUE 1945555039024054528
    MAXVALUE 1981583836043018239
    CACHE 1;
ALTER TABLE ta_bookmark_seq OWNER TO ncuser;
CREATE TABLE ta (
    _bookmark_ bigint DEFAULT nextval('ta_bookmark_seq'::regclass) NOT NULL,
    pgcd character varying,
    cocd character varying,
    lcn1 character varying,
    lcn2 character varying,
    rels character varying,
    alto integer,
    avto integer,
    sold integer,
    optn integer,
    over integer,
    resd integer,
    "DESC" character varying,
    bckt character varying,
    ctcd character varying,
    mfdt date,
    mfus character varying,
    mftm character varying
);
ALTER TABLE ta OWNER TO ncuser;
COMMENT ON TABLE ta IS 'Transport availability';
COMMENT ON COLUMN ta.pgcd IS 'Programme code';
COMMENT ON COLUMN ta.cocd IS 'Component code';
COMMENT ON COLUMN ta.lcn1 IS 'Departure point';
COMMENT ON COLUMN ta.lcn2 IS 'Arrival point';
COMMENT ON COLUMN ta.rels IS 'Resort list';
COMMENT ON COLUMN ta.alto IS 'Allocation total';
COMMENT ON COLUMN ta.avto IS 'Available total';
COMMENT ON COLUMN ta.sold IS 'Number sold';
COMMENT ON COLUMN ta.optn IS 'Number on option';
COMMENT ON COLUMN ta.over IS 'Overbooked';
COMMENT ON COLUMN ta.resd IS 'No reserved';
COMMENT ON COLUMN ta."DESC" IS 'Description';
COMMENT ON COLUMN ta.bckt IS 'Bucket (0=tot & 1-6)';
COMMENT ON COLUMN ta.ctcd IS 'Component type';
COMMENT ON COLUMN ta.mfdt IS 'Date Last Modified';
COMMENT ON COLUMN ta.mfus IS 'Modififed by Username';
COMMENT ON COLUMN ta.mftm IS 'Modified at Terminal';
CREATE SEQUENCE th_bookmark_seq
    START WITH 2197756618156802304
    INCREMENT BY 1
    MINVALUE 2197756618156802304
    MAXVALUE 2233785415175766015
    CACHE 1;
ALTER TABLE th_bookmark_seq OWNER TO ncuser;
CREATE TABLE th (
    _bookmark_ bigint DEFAULT nextval('th_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    thcd character varying,
    trid character varying,
    orjt character varying,
    stat character varying(1),
    crdt date,
    crtm character varying,
    crus character varying,
    dlus character varying,
    dldt date,
    dltm character varying,
    text character varying,
    objt character varying,
    thid character varying,
    jlid character varying,
    lock character varying(1),
    thdt date,
    thtm character varying,
    drid character varying,
    note character varying,
    "USER" character varying,
    wfid character varying,
    cltr integer,
    smlp character varying,
    cmlp character varying,
    stml character varying,
    clml character varying,
    subj character varying,
    mlto character varying,
    body character varying,
    attc character varying,
    sent character varying(1),
    ovrw character varying(1),
    ud01 character varying,
    ud02 character varying,
    ud03 character varying,
    ud04 character varying,
    ud05 character varying,
    ud06 character varying,
    ud07 character varying,
    ud08 character varying,
    ud09 character varying,
    ud10 character varying,
    ud11 character varying,
    ud12 character varying,
    ud13 character varying,
    ud14 character varying,
    ud15 character varying,
    ud16 character varying,
    ud17 character varying,
    husr character varying,
    tmid character varying,
    tact character varying(1),
    rtyp character varying,
    clid character varying,
    ousr character varying,
    agnt character varying,
    qtyp character varying,
    escl character varying(1),
    pdat date,
    ptim character varying,
    adat date,
    atim character varying,
    rdat date,
    rtim character varying,
    cdat date,
    ctim character varying,
    ud18 character varying,
    ud19 character varying,
    ud20 character varying,
    ud21 character varying,
    ud22 character varying,
    ud23 character varying,
    ud24 character varying,
    ud25 character varying,
    bmet character varying,
    trto character varying,
    type character varying,
    ud26 character varying,
    ud27 character varying,
    ud28 character varying,
    plac character varying,
    imgp character varying,
    vsno integer
);
ALTER TABLE th OWNER TO ncuser;
COMMENT ON TABLE th IS 'Tracking History';
COMMENT ON COLUMN th.crcd IS 'Courier Code';
COMMENT ON COLUMN th.thcd IS 'Tracking ref (object value)';
COMMENT ON COLUMN th.trid IS 'Tracking id';
COMMENT ON COLUMN th.orjt IS 'Original transaction id';
COMMENT ON COLUMN th.stat IS 'Status';
COMMENT ON COLUMN th.crdt IS 'Created date';
COMMENT ON COLUMN th.crtm IS 'Created time';
COMMENT ON COLUMN th.crus IS 'Created user id';
COMMENT ON COLUMN th.dlus IS 'Deleted user id';
COMMENT ON COLUMN th.dldt IS 'Deleted date';
COMMENT ON COLUMN th.dltm IS 'Deleted time';
COMMENT ON COLUMN th.text IS 'Text';
COMMENT ON COLUMN th.objt IS 'Object type';
COMMENT ON COLUMN th.thid IS 'Job tracking id';
COMMENT ON COLUMN th.jlid IS 'Job leg id';
COMMENT ON COLUMN th.lock IS 'Locked';
COMMENT ON COLUMN th.thdt IS 'Tracking Date';
COMMENT ON COLUMN th.thtm IS 'Tracking Time';
COMMENT ON COLUMN th.drid IS 'Driver Id';
COMMENT ON COLUMN th.note IS 'Additional Notes';
COMMENT ON COLUMN th."USER" IS 'Tracking by User Id';
COMMENT ON COLUMN th.wfid IS 'Work flow id';
COMMENT ON COLUMN th.cltr IS 'Client tracking weight';
COMMENT ON COLUMN th.smlp IS 'Set milestone prompt';
COMMENT ON COLUMN th.cmlp IS 'Cleared milestone prompt';
COMMENT ON COLUMN th.stml IS 'Set status field name';
COMMENT ON COLUMN th.clml IS 'Clear status field name';
COMMENT ON COLUMN th.subj IS 'Mail subject';
COMMENT ON COLUMN th.mlto IS 'Mail to';
COMMENT ON COLUMN th.body IS 'Mail body';
COMMENT ON COLUMN th.attc IS 'Mail attachments';
COMMENT ON COLUMN th.sent IS 'Email sent';
COMMENT ON COLUMN th.ovrw IS 'Overwritten';
COMMENT ON COLUMN th.ud01 IS 'User defined field 1';
COMMENT ON COLUMN th.ud02 IS 'User defined field 2';
COMMENT ON COLUMN th.ud03 IS 'User defined field 3';
COMMENT ON COLUMN th.ud04 IS 'User defined field 4';
COMMENT ON COLUMN th.ud05 IS 'User defined field 5';
COMMENT ON COLUMN th.ud06 IS 'User defined field 6';
COMMENT ON COLUMN th.ud07 IS 'User defined field 7';
COMMENT ON COLUMN th.ud08 IS 'User defined field 8';
COMMENT ON COLUMN th.ud09 IS 'User defined field 9';
COMMENT ON COLUMN th.ud10 IS 'User defined field 10';
COMMENT ON COLUMN th.ud11 IS 'User defined field 11';
COMMENT ON COLUMN th.ud12 IS 'User defined field 12';
COMMENT ON COLUMN th.ud13 IS 'User defined field 13';
COMMENT ON COLUMN th.ud14 IS 'User defined field 14';
COMMENT ON COLUMN th.ud15 IS 'User defined field 15';
COMMENT ON COLUMN th.ud16 IS 'User defined field 16';
COMMENT ON COLUMN th.ud17 IS 'User defined field 17';
COMMENT ON COLUMN th.husr IS 'Handheld user';
COMMENT ON COLUMN th.tmid IS 'Transmission ID';
COMMENT ON COLUMN th.tact IS 'Trans Action Add or Delete';
COMMENT ON COLUMN th.rtyp IS 'Record type';
COMMENT ON COLUMN th.clid IS 'Client Id';
COMMENT ON COLUMN th.ousr IS 'Originating User';
COMMENT ON COLUMN th.agnt IS 'Agent - DX user';
COMMENT ON COLUMN th.qtyp IS 'Query type';
COMMENT ON COLUMN th.escl IS 'Escalation (Y/N)';
COMMENT ON COLUMN th.pdat IS 'Post date';
COMMENT ON COLUMN th.ptim IS 'Post time';
COMMENT ON COLUMN th.adat IS 'Assign date';
COMMENT ON COLUMN th.atim IS 'Assign time';
COMMENT ON COLUMN th.rdat IS 'Reply date';
COMMENT ON COLUMN th.rtim IS 'Reply time';
COMMENT ON COLUMN th.cdat IS 'Close date';
COMMENT ON COLUMN th.ctim IS 'Close time';
COMMENT ON COLUMN th.ud18 IS 'User defined field 18';
COMMENT ON COLUMN th.ud19 IS 'User defined field 19';
COMMENT ON COLUMN th.ud20 IS 'User defined field 20';
COMMENT ON COLUMN th.ud21 IS 'User defined field 21';
COMMENT ON COLUMN th.ud22 IS 'User defined field 22';
COMMENT ON COLUMN th.ud23 IS 'User defined field 23';
COMMENT ON COLUMN th.ud24 IS 'User defined field 24';
COMMENT ON COLUMN th.ud25 IS 'User defined field 25';
COMMENT ON COLUMN th.bmet IS 'Booking method';
COMMENT ON COLUMN th.trto IS 'Tracked to';
COMMENT ON COLUMN th.type IS 'Tracking line type (N/T)';
COMMENT ON COLUMN th.ud26 IS 'User defined field 26';
COMMENT ON COLUMN th.ud27 IS 'User defined field 27';
COMMENT ON COLUMN th.ud28 IS 'User defined field 28';
COMMENT ON COLUMN th.plac IS 'Event place';
COMMENT ON COLUMN th.imgp IS 'Image path';
CREATE SEQUENCE ti_bookmark_seq
    START WITH 1801439850948198656
    INCREMENT BY 1
    MINVALUE 1801439850948198656
    MAXVALUE 1837468647967162367
    CACHE 1;
ALTER TABLE ti_bookmark_seq OWNER TO ncuser;
CREATE TABLE ti (
    _bookmark_ bigint DEFAULT nextval('ti_bookmark_seq'::regclass) NOT NULL,
    stck character varying,
    ticd character varying,
    inst character varying,
    cost numeric(10,2),
    dati date,
    dato date,
    clcd character varying,
    bocd character varying,
    btcd character varying,
    lead character varying,
    phcd character varying,
    ocar character varying,
    obal integer,
    odlc character varying,
    odld character varying,
    oalc character varying,
    oald character varying,
    otno character varying,
    oddt date,
    odtm character varying,
    oatm character varying,
    icar character varying,
    ibal integer,
    idlc character varying,
    idld character varying,
    ialc character varying,
    iald character varying,
    itno character varying,
    iddt date,
    idtm character varying,
    iatm character varying,
    omad integer,
    ofad integer,
    ochl integer,
    oinf integer,
    imad integer,
    ifad integer,
    ichl integer,
    iinf integer,
    ctcd character varying,
    pgcd character varying,
    cocd character varying,
    vnum character varying,
    date date,
    uscd character varying,
    drcd character varying,
    docd character varying,
    code character varying,
    oadt integer,
    bxcd character varying,
    bxnm character varying,
    csds date,
    csdl date,
    phde character varying,
    "DESC" character varying,
    bldg character varying,
    strt character varying,
    area character varying,
    city character varying,
    ctry character varying,
    bxls character varying,
    crem character varying,
    otrm character varying,
    itrm character varying,
    supp character varying,
    pnam character varying,
    pstr character varying,
    pare character varying,
    pcit character varying,
    dept character varying,
    cd01 character varying,
    cd02 character varying,
    cd03 character varying,
    cd04 character varying,
    cdes character varying,
    supn character varying,
    iccd character varying,
    numb character varying,
    pbld character varying,
    ppco character varying,
    occd character varying,
    icno integer,
    ocno integer,
    ctds character varying,
    osup character varying,
    ofln character varying,
    ocls character varying,
    odet character varying,
    oart character varying,
    isup character varying,
    ifln character varying,
    icls character varying,
    idet character varying,
    iart character varying,
    oldd date,
    ocit character varying,
    icit character varying,
    ildd date
);
ALTER TABLE ti OWNER TO ncuser;
COMMENT ON TABLE ti IS 'Ticket file';
COMMENT ON COLUMN ti.stck IS 'Stock code';
COMMENT ON COLUMN ti.ticd IS 'Ticket number';
COMMENT ON COLUMN ti.inst IS 'In stock key (stck dayr numb)';
COMMENT ON COLUMN ti.cost IS 'Cost price';
COMMENT ON COLUMN ti.dati IS 'Date received';
COMMENT ON COLUMN ti.dato IS 'Date despatched';
COMMENT ON COLUMN ti.clcd IS 'Client code';
COMMENT ON COLUMN ti.bocd IS 'Booking number';
COMMENT ON COLUMN ti.btcd IS 'Booking transaction';
COMMENT ON COLUMN ti.lead IS 'Lead name';
COMMENT ON COLUMN ti.phcd IS 'Package header code';
COMMENT ON COLUMN ti.ocar IS 'Outbound carrier';
COMMENT ON COLUMN ti.obal IS 'Outbound baggage allowance';
COMMENT ON COLUMN ti.odlc IS 'Outbound departure port code';
COMMENT ON COLUMN ti.odld IS 'Outbound departure port desc';
COMMENT ON COLUMN ti.oalc IS 'Outbound arrival port code';
COMMENT ON COLUMN ti.oald IS 'Outbound arrival port desc';
COMMENT ON COLUMN ti.otno IS 'Outbound transport number';
COMMENT ON COLUMN ti.oddt IS 'Outbound departure date';
COMMENT ON COLUMN ti.odtm IS 'Outbound departure time';
COMMENT ON COLUMN ti.oatm IS 'Outbound arrival time';
COMMENT ON COLUMN ti.icar IS 'Inbound carrier';
COMMENT ON COLUMN ti.ibal IS 'Inbound baggage allowance';
COMMENT ON COLUMN ti.idlc IS 'Inbound departure port code';
COMMENT ON COLUMN ti.idld IS 'Inbound departure port desc';
COMMENT ON COLUMN ti.ialc IS 'Inbound arrival location code';
COMMENT ON COLUMN ti.iald IS 'Inbound arrival location desc';
COMMENT ON COLUMN ti.itno IS 'Inbound transport number';
COMMENT ON COLUMN ti.iddt IS 'Inbound departure date';
COMMENT ON COLUMN ti.idtm IS 'Inbound departure time';
COMMENT ON COLUMN ti.iatm IS 'Inbound arrival time';
COMMENT ON COLUMN ti.omad IS 'Outbound male adults';
COMMENT ON COLUMN ti.ofad IS 'Outbound female adults';
COMMENT ON COLUMN ti.ochl IS 'Outbound children';
COMMENT ON COLUMN ti.oinf IS 'Outbound infants';
COMMENT ON COLUMN ti.imad IS 'Inbound male adults';
COMMENT ON COLUMN ti.ifad IS 'Inbound female adults';
COMMENT ON COLUMN ti.ichl IS 'Inbound children';
COMMENT ON COLUMN ti.iinf IS 'Inbound infants';
COMMENT ON COLUMN ti.ctcd IS 'Component type';
COMMENT ON COLUMN ti.pgcd IS 'Programme';
COMMENT ON COLUMN ti.cocd IS 'Component';
COMMENT ON COLUMN ti.vnum IS 'Voucher number';
COMMENT ON COLUMN ti.date IS 'Date created';
COMMENT ON COLUMN ti.uscd IS 'User';
COMMENT ON COLUMN ti.drcd IS 'Document request code';
COMMENT ON COLUMN ti.docd IS 'Document code';
COMMENT ON COLUMN ti.code IS 'Human readable booking number';
COMMENT ON COLUMN ti.oadt IS 'Outbound adults (total)';
COMMENT ON COLUMN ti.bxcd IS 'Passenger code';
COMMENT ON COLUMN ti.bxnm IS 'Passenger name';
COMMENT ON COLUMN ti.csds IS 'Component start date (short)';
COMMENT ON COLUMN ti.csdl IS 'Component start date (long)';
COMMENT ON COLUMN ti.phde IS 'Package header description';
COMMENT ON COLUMN ti."DESC" IS 'Customer description';
COMMENT ON COLUMN ti.bldg IS 'Customer building';
COMMENT ON COLUMN ti.strt IS 'Customer street';
COMMENT ON COLUMN ti.area IS 'customer area';
COMMENT ON COLUMN ti.city IS 'Customer city';
COMMENT ON COLUMN ti.ctry IS 'Customer country';
COMMENT ON COLUMN ti.bxls IS 'Passenger list';
COMMENT ON COLUMN ti.crem IS 'Expanded codified remarks';
COMMENT ON COLUMN ti.otrm IS 'Outbound terminal';
COMMENT ON COLUMN ti.itrm IS 'Inbound terminal';
COMMENT ON COLUMN ti.supp IS 'voucher supplement field';
COMMENT ON COLUMN ti.pnam IS 'Property name';
COMMENT ON COLUMN ti.pstr IS 'Property street address';
COMMENT ON COLUMN ti.pare IS 'Property area address';
COMMENT ON COLUMN ti.pcit IS 'Property city address';
COMMENT ON COLUMN ti.dept IS 'Departure time';
COMMENT ON COLUMN ti.cd01 IS 'component confirmation descr.1';
COMMENT ON COLUMN ti.cd02 IS 'component confirmation descr.2';
COMMENT ON COLUMN ti.cd03 IS 'component confirmation descr.3';
COMMENT ON COLUMN ti.cd04 IS 'component confirmation descr.4';
COMMENT ON COLUMN ti.cdes IS 'description for subst routine';
COMMENT ON COLUMN ti.supn IS 'supplier number';
COMMENT ON COLUMN ti.iccd IS 'Inbound Carrier code';
COMMENT ON COLUMN ti.numb IS 'Predefined ticket number';
COMMENT ON COLUMN ti.pbld IS 'Building';
COMMENT ON COLUMN ti.ppco IS 'Post Code';
COMMENT ON COLUMN ti.occd IS 'outbound carrier code';
COMMENT ON COLUMN ti.icno IS 'inbound carrier number';
COMMENT ON COLUMN ti.ocno IS 'outbout carrier code';
COMMENT ON COLUMN ti.ctds IS 'Component Type description';
COMMENT ON COLUMN ti.osup IS 'Outbound flight supplier';
COMMENT ON COLUMN ti.ofln IS 'Outbound flight num from BT';
COMMENT ON COLUMN ti.ocls IS 'Outbound component descriptn';
COMMENT ON COLUMN ti.odet IS 'Outbound depart time from BT';
COMMENT ON COLUMN ti.oart IS 'Outbound arrival time from BT';
COMMENT ON COLUMN ti.isup IS 'Inbound supplier from BT';
COMMENT ON COLUMN ti.ifln IS 'Inbound flight number from BT';
COMMENT ON COLUMN ti.icls IS 'Inbound component description';
COMMENT ON COLUMN ti.idet IS 'Inbound depart time from BT';
COMMENT ON COLUMN ti.iart IS 'Inbound arrival time from BT';
COMMENT ON COLUMN ti.oldd IS 'Outbound dep date long format';
COMMENT ON COLUMN ti.ocit IS 'Outbound check in time';
COMMENT ON COLUMN ti.icit IS 'Inbound check in time';
COMMENT ON COLUMN ti.ildd IS 'Inbound dep date long format';
CREATE SEQUENCE tr_bookmark_seq
    START WITH 2233785415175766272
    INCREMENT BY 1
    MINVALUE 2233785415175766272
    MAXVALUE 2269814212194729983
    CACHE 1;
ALTER TABLE tr_bookmark_seq OWNER TO ncuser;
CREATE TABLE tr (
    _bookmark_ bigint DEFAULT nextval('tr_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    trcd character varying,
    "DESC" character varying,
    stat character varying(1),
    vsno integer,
    trid character varying,
    wfid character varying,
    seqn character varying,
    stst character varying,
    stml character varying,
    clml character varying,
    tbas character varying,
    "time" integer,
    cltr integer,
    batc character varying(1),
    tcod character varying,
    ana1 character varying,
    ana2 character varying
);
ALTER TABLE tr OWNER TO ncuser;
COMMENT ON TABLE tr IS 'Tracking Maintenance';
COMMENT ON COLUMN tr.crcd IS 'Courier Code';
COMMENT ON COLUMN tr.trcd IS 'Tracking Code';
COMMENT ON COLUMN tr."DESC" IS 'Tracking Description';
COMMENT ON COLUMN tr.stat IS 'Record Status';
COMMENT ON COLUMN tr.vsno IS 'Version No.';
COMMENT ON COLUMN tr.trid IS 'Tracking Maintenance Id';
COMMENT ON COLUMN tr.wfid IS 'Workflow id';
COMMENT ON COLUMN tr.seqn IS 'Sequence';
COMMENT ON COLUMN tr.stst IS 'Set status to';
COMMENT ON COLUMN tr.stml IS 'Set Milestone to';
COMMENT ON COLUMN tr.clml IS 'Clear milestone';
COMMENT ON COLUMN tr.tbas IS 'Time basis';
COMMENT ON COLUMN tr."time" IS 'Time in minute';
COMMENT ON COLUMN tr.cltr IS 'Client tracking weight';
COMMENT ON COLUMN tr.batc IS 'Is a batch tracking';
COMMENT ON COLUMN tr.tcod IS 'Third party code';
COMMENT ON COLUMN tr.ana1 IS 'Job analysis code 1';
COMMENT ON COLUMN tr.ana2 IS 'Job analysis code 2';
CREATE SEQUENCE tx_bookmark_seq
    START WITH 2269814212194730240
    INCREMENT BY 1
    MINVALUE 2269814212194730240
    MAXVALUE 2305843009213693951
    CACHE 1;
ALTER TABLE tx_bookmark_seq OWNER TO ncuser;
CREATE TABLE tx (
    _bookmark_ bigint DEFAULT nextval('tx_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    txcd character varying,
    "DESC" character varying,
    inco character varying,
    rate numeric(8,2),
    stat character varying(1),
    vsno integer,
    txid character varying,
    bicu character varying,
    sicu character varying
);
ALTER TABLE tx OWNER TO ncuser;
COMMENT ON TABLE tx IS 'Sales Tax';
COMMENT ON COLUMN tx.crcd IS 'Courier Code';
COMMENT ON COLUMN tx.txcd IS 'Sales Tax Short Name';
COMMENT ON COLUMN tx."DESC" IS 'Sales Tax Description';
COMMENT ON COLUMN tx.inco IS 'Included Countries';
COMMENT ON COLUMN tx.rate IS 'Rate';
COMMENT ON COLUMN tx.stat IS 'Status';
COMMENT ON COLUMN tx.vsno IS 'Version No.';
COMMENT ON COLUMN tx.txid IS 'Sales Tax Id';
COMMENT ON COLUMN tx.bicu IS 'Big Icon URL';
COMMENT ON COLUMN tx.sicu IS 'Small Icon URL';
CREATE SEQUENCE uc_bookmark_seq
    START WITH 3386706919782613248
    INCREMENT BY 1
    MINVALUE 3386706919782613248
    MAXVALUE 3422735716801576959
    CACHE 1;
ALTER TABLE uc_bookmark_seq OWNER TO ncuser;
CREATE TABLE uc (
    _bookmark_ bigint DEFAULT nextval('uc_bookmark_seq'::regclass) NOT NULL,
    uscd character varying,
    year integer,
    week integer,
    wgcd character varying,
    dvcd character varying,
    ntbo integer,
    ntbx integer,
    targ numeric(10,2),
    pafc numeric(10,2),
    pacc numeric(10,2),
    pain numeric(10,2),
    pach numeric(10,2),
    paac numeric(10,2),
    padc numeric(10,2),
    niab integer,
    ninb integer,
    nacb integer,
    ncab integer,
    ndcb integer,
    noip integer,
    ltpw integer,
    lttw integer,
    retn numeric(7,2),
    cmpd numeric(10,2),
    cmtp numeric(10,2),
    diyr character varying,
    wkco date,
    type character varying(1)
);
ALTER TABLE uc OWNER TO ncuser;
COMMENT ON TABLE uc IS 'User commission';
COMMENT ON COLUMN uc.uscd IS 'User code';
COMMENT ON COLUMN uc.year IS 'Year';
COMMENT ON COLUMN uc.week IS 'Week number';
COMMENT ON COLUMN uc.wgcd IS 'Workgroup';
COMMENT ON COLUMN uc.dvcd IS 'Division code';
COMMENT ON COLUMN uc.ntbo IS 'Number of bookings';
COMMENT ON COLUMN uc.ntbx IS 'Number of passengers';
COMMENT ON COLUMN uc.targ IS 'Weekly/monthly profit target';
COMMENT ON COLUMN uc.pafc IS 'Profit achieved - flights';
COMMENT ON COLUMN uc.pacc IS 'Profit achieved - cash stash';
COMMENT ON COLUMN uc.pain IS 'Profit achieved - insurance';
COMMENT ON COLUMN uc.pach IS 'Profit achieved - car hire';
COMMENT ON COLUMN uc.paac IS 'Profit achieved - accomm';
COMMENT ON COLUMN uc.padc IS 'Profit achieved - debit cards';
COMMENT ON COLUMN uc.niab IS 'Num of IATA bookings';
COMMENT ON COLUMN uc.ninb IS 'Num of insurance bookings';
COMMENT ON COLUMN uc.nacb IS 'Num of accomm bookings';
COMMENT ON COLUMN uc.ncab IS 'Num car hire bookings';
COMMENT ON COLUMN uc.ndcb IS 'Num debit card bookings';
COMMENT ON COLUMN uc.noip IS 'Num insurance passengers';
COMMENT ON COLUMN uc.ltpw IS 'League table pos last week';
COMMENT ON COLUMN uc.lttw IS 'League table pos this week';
COMMENT ON COLUMN uc.retn IS 'Retention percentage';
COMMENT ON COLUMN uc.cmpd IS 'Commission paid to date';
COMMENT ON COLUMN uc.cmtp IS 'Commission to be paid';
COMMENT ON COLUMN uc.diyr IS 'Fiscal year display format';
COMMENT ON COLUMN uc.wkco IS 'Week commencing date';
COMMENT ON COLUMN uc.type IS 'Type (weekly or monthly)';
CREATE SEQUENCE ud_bookmark_seq
    START WITH 3278620528725721344
    INCREMENT BY 1
    MINVALUE 3278620528725721344
    MAXVALUE 3314649325744685055
    CACHE 1;
ALTER TABLE ud_bookmark_seq OWNER TO ncuser;
CREATE TABLE ud (
    _bookmark_ bigint DEFAULT nextval('ud_bookmark_seq'::regclass) NOT NULL,
    adin character varying,
    crcd character varying,
    file character varying,
    stat character varying(1),
    udfn character varying,
    udfv character varying,
    udid character varying,
    vsno integer
);
ALTER TABLE ud OWNER TO ncuser;
COMMENT ON TABLE ud IS 'User Defined File';
COMMENT ON COLUMN ud.adin IS 'Additional Info';
COMMENT ON COLUMN ud.crcd IS 'Courier Code';
COMMENT ON COLUMN ud.file IS 'File need to be extended';
COMMENT ON COLUMN ud.stat IS 'Status';
COMMENT ON COLUMN ud.udfn IS 'User Defined Field Name';
COMMENT ON COLUMN ud.udfv IS 'User Defined Field Value';
COMMENT ON COLUMN ud.udid IS 'Internal No';
COMMENT ON COLUMN ud.vsno IS 'Version No';
CREATE SEQUENCE us_bookmark_seq
    START WITH 2305843009213694208
    INCREMENT BY 1
    MINVALUE 2305843009213694208
    MAXVALUE 2341871806232657919
    CACHE 1;
ALTER TABLE us_bookmark_seq OWNER TO ncuser;
CREATE TABLE us (
    _bookmark_ bigint DEFAULT nextval('us_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    uscd character varying,
    pass character varying,
    "DESC" character varying,
    telm character varying,
    telh character varying,
    telo character varying,
    extn character varying,
    emal character varying,
    mod1 character varying,
    mod2 character varying,
    prvl character varying,
    note character varying,
    stat character varying(1),
    ctfl character varying,
    clid character varying,
    agcd character varying,
    sspc character varying(1),
    scpc character varying(1),
    vsno integer,
    usid character varying,
    bicu character varying,
    sicu character varying,
    pdid character varying,
    rol0 character varying(1),
    rol1 character varying(1),
    rol2 character varying(1),
    rol3 character varying(1),
    rol4 character varying(1),
    rol5 character varying(1),
    rol6 character varying(1),
    rows integer,
    novc character varying(1),
    auto character varying(1),
    type character varying(1),
    asub character varying(1),
    them character varying,
    dunt character varying,
    wunt character varying,
    uhwd character varying,
    cuid character varying,
    latd character varying,
    lond character varying,
    lupt character varying,
    latm character varying,
    lats bigint,
    cldt date,
    cltm character varying,
    lldt date,
    lltm character varying,
    opls character varying,
    autp character varying(1),
    pdat date,
    expd date,
    expt character varying,
    salt character varying
);
ALTER TABLE us OWNER TO ncuser;
COMMENT ON TABLE us IS 'User File';
COMMENT ON COLUMN us.crcd IS 'Courier Code';
COMMENT ON COLUMN us.uscd IS 'User Short Name';
COMMENT ON COLUMN us.pass IS 'Password';
COMMENT ON COLUMN us."DESC" IS 'User Name';
COMMENT ON COLUMN us.telm IS 'Mobile Phone Number';
COMMENT ON COLUMN us.telh IS 'Home Phone Number';
COMMENT ON COLUMN us.telo IS 'Office Phone Number';
COMMENT ON COLUMN us.extn IS 'Extension';
COMMENT ON COLUMN us.emal IS 'Contact Email Address';
COMMENT ON COLUMN us.mod1 IS 'Frequently Used 1';
COMMENT ON COLUMN us.mod2 IS 'Frequently Used 2';
COMMENT ON COLUMN us.prvl IS 'User Privilege';
COMMENT ON COLUMN us.note IS 'Note';
COMMENT ON COLUMN us.stat IS 'Record Status';
COMMENT ON COLUMN us.ctfl IS 'Controller Field Selection';
COMMENT ON COLUMN us.clid IS 'Customer Id';
COMMENT ON COLUMN us.agcd IS 'Agent Id';
COMMENT ON COLUMN us.sspc IS 'Show Sales Price';
COMMENT ON COLUMN us.scpc IS 'Show Cost Price';
COMMENT ON COLUMN us.vsno IS 'Version No.';
COMMENT ON COLUMN us.usid IS 'User Id';
COMMENT ON COLUMN us.bicu IS 'Big Icon URL';
COMMENT ON COLUMN us.sicu IS 'Small Icon URL';
COMMENT ON COLUMN us.pdid IS 'Product ID';
COMMENT ON COLUMN us.rol0 IS 'Metafour System User';
COMMENT ON COLUMN us.rol1 IS 'System manager';
COMMENT ON COLUMN us.rol2 IS 'Role 2';
COMMENT ON COLUMN us.rol3 IS 'Role 3';
COMMENT ON COLUMN us.rol4 IS 'Role 4';
COMMENT ON COLUMN us.rol5 IS 'Role 5';
COMMENT ON COLUMN us.rol6 IS 'Role 6';
COMMENT ON COLUMN us.rows IS 'No of records to display';
COMMENT ON COLUMN us.novc IS 'Novice flag';
COMMENT ON COLUMN us.auto IS 'Turn on auto complete in booking screen';
COMMENT ON COLUMN us.type IS 'User type';
COMMENT ON COLUMN us.asub IS 'Can add jobs for sub client';
COMMENT ON COLUMN us.them IS 'Theme name';
COMMENT ON COLUMN us.dunt IS 'Distance unit';
COMMENT ON COLUMN us.wunt IS 'Weight unit';
COMMENT ON COLUMN us.uhwd IS 'Unit of length (Dimension)';
COMMENT ON COLUMN us.cuid IS 'Currency';
COMMENT ON COLUMN us.latd IS 'Latitude';
COMMENT ON COLUMN us.lond IS 'Longitude';
COMMENT ON COLUMN us.lupt IS 'Last update time';
COMMENT ON COLUMN us.latm IS 'Last transmission ID';
COMMENT ON COLUMN us.lats IS 'Last tracking received';
COMMENT ON COLUMN us.cldt IS 'Current login date';
COMMENT ON COLUMN us.cltm IS 'Current login time';
COMMENT ON COLUMN us.lldt IS 'Last login date';
COMMENT ON COLUMN us.lltm IS 'Last login time';
COMMENT ON COLUMN us.opls IS 'Last password lists';
COMMENT ON COLUMN us.autp IS 'Auto generated pass';
COMMENT ON COLUMN us.pdat IS 'Last password change date';
COMMENT ON COLUMN us.expd IS 'Token expiry date';
COMMENT ON COLUMN us.expt IS 'Token expiry time';
CREATE SEQUENCE wa_bookmark_seq
    START WITH 2089670227099910400
    INCREMENT BY 1
    MINVALUE 2089670227099910400
    MAXVALUE 2125699024118874111
    CACHE 1;
ALTER TABLE wa_bookmark_seq OWNER TO ncuser;
CREATE TABLE wa (
    _bookmark_ bigint DEFAULT nextval('wa_bookmark_seq'::regclass) NOT NULL,
    pgcd character varying,
    ctcd character varying,
    arrv character varying,
    durn character varying,
    prty character varying,
    cocd character varying,
    dat1 date,
    dat2 date,
    fsls character varying,
    fsd1 date,
    fsd2 date,
    cols character varying,
    recd character varying,
    "DESC" character varying,
    dayw character varying,
    atcd character varying,
    mfdt date,
    mfus character varying,
    mftm character varying
);
ALTER TABLE wa OWNER TO ncuser;
COMMENT ON TABLE wa IS 'Weekly availability';
COMMENT ON COLUMN wa.pgcd IS 'Programme';
COMMENT ON COLUMN wa.ctcd IS 'Component type';
COMMENT ON COLUMN wa.arrv IS 'Destination';
COMMENT ON COLUMN wa.durn IS 'Duration';
COMMENT ON COLUMN wa.prty IS 'Priority';
COMMENT ON COLUMN wa.cocd IS 'Component';
COMMENT ON COLUMN wa.dat1 IS 'First date';
COMMENT ON COLUMN wa.dat2 IS 'Last date';
COMMENT ON COLUMN wa.fsls IS 'Flights stream list';
COMMENT ON COLUMN wa.fsd1 IS 'Flights stream date 1';
COMMENT ON COLUMN wa.fsd2 IS 'Flight stream date 2';
COMMENT ON COLUMN wa.cols IS 'Component list';
COMMENT ON COLUMN wa.recd IS 'Resort code';
COMMENT ON COLUMN wa."DESC" IS 'Description';
COMMENT ON COLUMN wa.dayw IS 'Day';
COMMENT ON COLUMN wa.atcd IS 'Accommodation type code';
COMMENT ON COLUMN wa.mfdt IS 'Date last modified';
COMMENT ON COLUMN wa.mfus IS 'Modified by user';
COMMENT ON COLUMN wa.mftm IS 'Modified at Terminal';
CREATE SEQUENCE we_bookmark_seq
    START WITH 2341871806232658176
    INCREMENT BY 1
    MINVALUE 2341871806232658176
    MAXVALUE 2377900603251621887
    CACHE 1;
ALTER TABLE we_bookmark_seq OWNER TO ncuser;
CREATE TABLE we (
    _bookmark_ bigint DEFAULT nextval('we_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    csid character varying,
    weid character varying,
    widt numeric(8,2),
    orjt character varying,
    stat character varying(1),
    seqn character varying,
    high numeric(8,2),
    dept numeric(8,2),
    item integer,
    vsno integer,
    crus character varying,
    crdt date,
    crtm character varying,
    dlus character varying,
    dldt date,
    dltm character varying,
    lock character varying(1),
    "DESC" character varying,
    ival numeric(8,2),
    tval numeric(8,2),
    wecd character varying,
    tpcd character varying,
    ltrk character varying,
    dlto character varying,
    podt date,
    potm character varying,
    ud01 character varying,
    ud02 character varying,
    ud03 character varying,
    ud04 character varying,
    ud05 character varying,
    twgt numeric(9,3),
    bgid bigint,
    sbgd character varying,
    fbgd character varying,
    lttx character varying,
    skey character varying
);
ALTER TABLE we OWNER TO ncuser;
COMMENT ON TABLE we IS 'Weight Details Information';
COMMENT ON COLUMN we.crcd IS 'Courier Code';
COMMENT ON COLUMN we.csid IS 'Job id';
COMMENT ON COLUMN we.weid IS 'Piece id';
COMMENT ON COLUMN we.widt IS 'Width of the piece';
COMMENT ON COLUMN we.orjt IS 'Original transaction id';
COMMENT ON COLUMN we.stat IS 'Record Status';
COMMENT ON COLUMN we.seqn IS 'Sequence Number';
COMMENT ON COLUMN we.high IS 'Height of the piece';
COMMENT ON COLUMN we.dept IS 'Depth/Length of the piece';
COMMENT ON COLUMN we.item IS 'No of Items';
COMMENT ON COLUMN we.vsno IS 'Version No.';
COMMENT ON COLUMN we.crus IS 'Created user id';
COMMENT ON COLUMN we.crdt IS 'Created date';
COMMENT ON COLUMN we.crtm IS 'Created time';
COMMENT ON COLUMN we.dlus IS 'Deleted user id';
COMMENT ON COLUMN we.dldt IS 'Deleted date';
COMMENT ON COLUMN we.dltm IS 'Deleted time';
COMMENT ON COLUMN we.lock IS 'Locked';
COMMENT ON COLUMN we."DESC" IS 'Full Description of Goods';
COMMENT ON COLUMN we.ival IS 'Item Value';
COMMENT ON COLUMN we.tval IS 'Total Value for Customer';
COMMENT ON COLUMN we.wecd IS 'Piece code';
COMMENT ON COLUMN we.tpcd IS 'Third party piece code';
COMMENT ON COLUMN we.ltrk IS 'Last tracking text';
COMMENT ON COLUMN we.dlto IS 'Delivered to';
COMMENT ON COLUMN we.podt IS 'Delivery date';
COMMENT ON COLUMN we.potm IS 'Delivery time';
COMMENT ON COLUMN we.ud01 IS 'User defined field 1';
COMMENT ON COLUMN we.ud02 IS 'User defined field 2';
COMMENT ON COLUMN we.ud03 IS 'User defined field 3';
COMMENT ON COLUMN we.ud04 IS 'User defined field 4';
COMMENT ON COLUMN we.ud05 IS 'User defined field 5';
COMMENT ON COLUMN we.twgt IS 'Total weight';
COMMENT ON COLUMN we.sbgd IS 'Second bag code';
COMMENT ON COLUMN we.fbgd IS 'First bag code';
COMMENT ON COLUMN we.lttx IS 'Last tracking text';
CREATE SEQUENCE wf_bookmark_seq
    START WITH 4035225266123964672
    INCREMENT BY 1
    MINVALUE 4035225266123964672
    MAXVALUE 4071254063142928383
    CACHE 1;
ALTER TABLE wf_bookmark_seq OWNER TO ncuser;
CREATE TABLE wf (
    _bookmark_ bigint DEFAULT nextval('wf_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    wfid character varying,
    wfcd character varying,
    "DESC" character varying,
    stat character varying(1),
    vsno integer,
    pdid character varying,
    priv character varying
);
ALTER TABLE wf OWNER TO ncuser;
COMMENT ON TABLE wf IS 'Work flow file';
COMMENT ON COLUMN wf.crcd IS 'Courier code';
COMMENT ON COLUMN wf.wfid IS 'Unique id';
COMMENT ON COLUMN wf.wfcd IS 'Workflow code';
COMMENT ON COLUMN wf."DESC" IS 'Description';
COMMENT ON COLUMN wf.stat IS 'Status';
COMMENT ON COLUMN wf.vsno IS 'Version no.';
COMMENT ON COLUMN wf.pdid IS 'Product id';
COMMENT ON COLUMN wf.priv IS 'Privilege';
CREATE SEQUENCE wg_bookmark_seq
    START WITH 1873497444986126592
    INCREMENT BY 1
    MINVALUE 1873497444986126592
    MAXVALUE 1909526242005090303
    CACHE 1;
ALTER TABLE wg_bookmark_seq OWNER TO ncuser;
CREATE TABLE wg (
    _bookmark_ bigint DEFAULT nextval('wg_bookmark_seq'::regclass) NOT NULL,
    wgcd character varying,
    "DESC" character varying,
    usls character varying,
    delf character varying,
    mfdt date,
    mfus character varying,
    mftm character varying,
    dvcd character varying,
    merc character varying,
    worc numeric(7,2),
    mbud numeric(10,2),
    bonc numeric(7,2),
    uf01 character varying,
    uf02 character varying,
    uf03 character varying,
    uf04 character varying,
    uf05 character varying,
    uf06 character varying,
    uf07 character varying,
    uf08 character varying,
    uf09 character varying,
    uf10 character varying,
    uf11 character varying,
    uf12 character varying,
    uf13 character varying,
    uf14 character varying,
    uf15 character varying,
    uf16 character varying,
    uf17 character varying,
    uf18 character varying,
    uf19 character varying,
    uf20 character varying,
    qpdf character varying,
    cupb character varying,
    lain character varying,
    larc character varying,
    pbpr character varying,
    pawg character varying
);
ALTER TABLE wg OWNER TO ncuser;
COMMENT ON TABLE wg IS 'Work group';
COMMENT ON COLUMN wg.wgcd IS 'Workgroup code';
COMMENT ON COLUMN wg."DESC" IS 'Workgroup description';
COMMENT ON COLUMN wg.usls IS 'List of members';
COMMENT ON COLUMN wg.delf IS 'Deleted flag';
COMMENT ON COLUMN wg.mfdt IS 'Date last altered';
COMMENT ON COLUMN wg.mfus IS 'User last altered';
COMMENT ON COLUMN wg.mftm IS 'Device last changed at';
COMMENT ON COLUMN wg.dvcd IS 'Division code';
COMMENT ON COLUMN wg.merc IS 'Merchant number';
COMMENT ON COLUMN wg.worc IS 'Weekly override commission';
COMMENT ON COLUMN wg.mbud IS 'Monthly budget';
COMMENT ON COLUMN wg.bonc IS 'Bonus commission';
COMMENT ON COLUMN wg.uf01 IS 'User field 01';
COMMENT ON COLUMN wg.uf02 IS 'User field 02';
COMMENT ON COLUMN wg.uf03 IS 'User field 03';
COMMENT ON COLUMN wg.uf04 IS 'User field 04';
COMMENT ON COLUMN wg.uf05 IS 'User field 05';
COMMENT ON COLUMN wg.uf06 IS 'User field 06';
COMMENT ON COLUMN wg.uf07 IS 'User field 07';
COMMENT ON COLUMN wg.uf08 IS 'User field 08';
COMMENT ON COLUMN wg.uf09 IS 'User field 09';
COMMENT ON COLUMN wg.uf10 IS 'User field 10';
COMMENT ON COLUMN wg.uf11 IS 'User field 11';
COMMENT ON COLUMN wg.uf12 IS 'User field 12';
COMMENT ON COLUMN wg.uf13 IS 'User field 13';
COMMENT ON COLUMN wg.uf14 IS 'User field 14';
COMMENT ON COLUMN wg.uf15 IS 'User field 15';
COMMENT ON COLUMN wg.uf16 IS 'User field 16';
COMMENT ON COLUMN wg.uf17 IS 'User field 17';
COMMENT ON COLUMN wg.uf18 IS 'User field 18';
COMMENT ON COLUMN wg.uf19 IS 'User field 19';
COMMENT ON COLUMN wg.uf20 IS 'User field 20';
COMMENT ON COLUMN wg.qpdf IS 'Default PDF queue';
COMMENT ON COLUMN wg.cupb IS 'Current period book';
COMMENT ON COLUMN wg.lain IS 'Last Invoice Line';
COMMENT ON COLUMN wg.larc IS 'Last Receipt Line';
COMMENT ON COLUMN wg.pbpr IS 'Period Book Prefix';
COMMENT ON COLUMN wg.pawg IS 'Parent Workgroup';
CREATE SEQUENCE xp_bookmark_seq
    START WITH 1909526242005090560
    INCREMENT BY 1
    MINVALUE 1909526242005090560
    MAXVALUE 1945555039024054271
    CACHE 1;
ALTER TABLE xp_bookmark_seq OWNER TO ncuser;
CREATE SEQUENCE za_bookmark_seq
    START WITH 3530822107858469120
    INCREMENT BY 1
    MINVALUE 3530822107858469120
    MAXVALUE 3566850904877432831
    CACHE 1;
ALTER TABLE za_bookmark_seq OWNER TO ncuser;
CREATE TABLE za (
    _bookmark_ bigint DEFAULT nextval('za_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    znid character varying,
    plid character varying,
    type character varying(2),
    vsno integer,
    tnam character varying,
    stat character varying(1)
);
ALTER TABLE za OWNER TO ncuser;
COMMENT ON TABLE za IS 'Zone Allocation';
COMMENT ON COLUMN za.crcd IS 'Courier code';
COMMENT ON COLUMN za.znid IS 'Zone id';
COMMENT ON COLUMN za.plid IS 'Place id';
COMMENT ON COLUMN za.type IS 'Reference type';
COMMENT ON COLUMN za.vsno IS 'Version no';
COMMENT ON COLUMN za.tnam IS 'Type Name';
COMMENT ON COLUMN za.stat IS 'Status';
CREATE SEQUENCE zg_bookmark_seq
    START WITH 3891110078048108800
    INCREMENT BY 1
    MINVALUE 3891110078048108800
    MAXVALUE 3927138875067072511
    CACHE 1;
ALTER TABLE zg_bookmark_seq OWNER TO ncuser;
CREATE TABLE zg (
    _bookmark_ bigint DEFAULT nextval('zg_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    zgid character varying,
    "DESC" character varying,
    stat character varying(1),
    prio integer,
    vsno integer
);
ALTER TABLE zg OWNER TO ncuser;
COMMENT ON TABLE zg IS 'Zone group';
COMMENT ON COLUMN zg.crcd IS 'Courier code';
COMMENT ON COLUMN zg.zgid IS 'Zone group Id';
COMMENT ON COLUMN zg."DESC" IS 'Zone group description';
COMMENT ON COLUMN zg.stat IS 'Status';
COMMENT ON COLUMN zg.prio IS 'Priority';
COMMENT ON COLUMN zg.vsno IS 'Version no.';
CREATE SEQUENCE zn_bookmark_seq
    START WITH 3494793310839505152
    INCREMENT BY 1
    MINVALUE 3494793310839505152
    MAXVALUE 3530822107858468863
    CACHE 1;
ALTER TABLE zn_bookmark_seq OWNER TO ncuser;
CREATE TABLE zn (
    _bookmark_ bigint DEFAULT nextval('zn_bookmark_seq'::regclass) NOT NULL,
    crcd character varying,
    znid character varying,
    "DESC" character varying,
    zgid character varying,
    type character varying(2),
    stat character varying(1),
    tdes character varying,
    vsno integer
);
ALTER TABLE zn OWNER TO ncuser;
COMMENT ON TABLE zn IS 'Zone file';
COMMENT ON COLUMN zn.crcd IS 'Courier code';
COMMENT ON COLUMN zn.znid IS 'Unique id';
COMMENT ON COLUMN zn."DESC" IS 'Zone name';
COMMENT ON COLUMN zn.zgid IS 'Zone category';
COMMENT ON COLUMN zn.type IS 'Zone type';
COMMENT ON COLUMN zn.stat IS 'Status';
COMMENT ON COLUMN zn.tdes IS 'Type description';
COMMENT ON COLUMN zn.vsno IS 'Version no.';
ALTER TABLE ONLY bg ALTER COLUMN bgid SET DEFAULT nextval('bg_bgid_seq'::regclass);
ALTER TABLE ONLY search ALTER COLUMN id SET DEFAULT nextval('search_id_seq'::regclass);

SET search_path = extra, pg_catalog;

-- ------------------------------------------------------
ALTER TABLE ONLY im_batchstatus ADD CONSTRAINT batchstatus_pky PRIMARY KEY (id);
ALTER TABLE ONLY im_csvrow ADD CONSTRAINT im_csvrow_pkey PRIMARY KEY (id);
ALTER TABLE ONLY im_jobbatch ADD CONSTRAINT im_jobbatch_pkey PRIMARY KEY (id);
ALTER TABLE ONLY im_jobreport ADD CONSTRAINT im_jobreport_pkey PRIMARY KEY (id);
ALTER TABLE ONLY im_csvrow ADD CONSTRAINT unique_csvrow UNIQUE (batch, rowno);

SET search_path = public, pg_catalog;
ALTER TABLE ONLY ad ADD CONSTRAINT ad__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY an ADD CONSTRAINT an__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY ap ADD CONSTRAINT ap__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY at ADD CONSTRAINT at__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY au ADD CONSTRAINT au__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY bg ADD CONSTRAINT bg_pkey PRIMARY KEY (bgid);
ALTER TABLE ONLY bp ADD CONSTRAINT bp__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY bt ADD CONSTRAINT bt__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY cd ADD CONSTRAINT cd__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY ci ADD CONSTRAINT ci__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY cl ADD CONSTRAINT cl__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY co ADD CONSTRAINT co__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY cp ADD CONSTRAINT cp__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY cr ADD CONSTRAINT cr__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY cs ADD CONSTRAINT cs__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY ct ADD CONSTRAINT ct__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY cu ADD CONSTRAINT cu__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY cv ADD CONSTRAINT cv__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY cy ADD CONSTRAINT cy__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY dc ADD CONSTRAINT dc__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY di ADD CONSTRAINT di__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY ds ADD CONSTRAINT ds__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY dt ADD CONSTRAINT dt__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY ea ADD CONSTRAINT ea__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY ex ADD CONSTRAINT ex__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY fields ADD CONSTRAINT fields__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY files ADD CONSTRAINT files__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY gflds ADD CONSTRAINT gflds__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY helps ADD CONSTRAINT helps__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY ho ADD CONSTRAINT ho__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY ip ADD CONSTRAINT ip__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY it ADD CONSTRAINT it__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY ja ADD CONSTRAINT ja__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY jl ADD CONSTRAINT jl__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY jp ADD CONSTRAINT jp__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY keys ADD CONSTRAINT keys__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY lf ADD CONSTRAINT lf__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY lg ADD CONSTRAINT lg__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY lh ADD CONSTRAINT lh__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY lo ADD CONSTRAINT lo__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY ls ADD CONSTRAINT ls__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY menu ADD CONSTRAINT menu__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY mf ADD CONSTRAINT mf__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY mq ADD CONSTRAINT mq__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY ns ADD CONSTRAINT ns__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY nt ADD CONSTRAINT nt__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY pa ADD CONSTRAINT pa__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY pc ADD CONSTRAINT pc__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY pd ADD CONSTRAINT pd__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY pe ADD CONSTRAINT pe__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY phys ADD CONSTRAINT phys__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY pi ADD CONSTRAINT pi__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY pl ADD CONSTRAINT pl__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY pm ADD CONSTRAINT pm__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY pr ADD CONSTRAINT pr__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY proc ADD CONSTRAINT proc__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY rd ADD CONSTRAINT rd__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY re ADD CONSTRAINT re__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY reln ADD CONSTRAINT reln__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY rh ADD CONSTRAINT rh__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY rl ADD CONSTRAINT rl__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY rp ADD CONSTRAINT rp__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY rs ADD CONSTRAINT rs__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY rt ADD CONSTRAINT rt__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY sa ADD CONSTRAINT sa__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY sc ADD CONSTRAINT sc__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY se ADD CONSTRAINT se__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY search ADD CONSTRAINT search_key UNIQUE (recordid, recordtype);
ALTER TABLE ONLY search ADD CONSTRAINT search_pkey PRIMARY KEY (id);
ALTER TABLE ONLY sequence ADD CONSTRAINT sequence_pkey PRIMARY KEY (seq_name);
ALTER TABLE ONLY si ADD CONSTRAINT si__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY sm ADD CONSTRAINT sm__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY sq ADD CONSTRAINT sq__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY su ADD CONSTRAINT su__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY sy ADD CONSTRAINT sy__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY syst ADD CONSTRAINT syst__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY ta ADD CONSTRAINT ta__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY th ADD CONSTRAINT th__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY ti ADD CONSTRAINT ti__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY tr ADD CONSTRAINT tr__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY tx ADD CONSTRAINT tx__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY uc ADD CONSTRAINT uc__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY ud ADD CONSTRAINT ud__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY us ADD CONSTRAINT us__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY wa ADD CONSTRAINT wa__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY we ADD CONSTRAINT we__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY wf ADD CONSTRAINT wf__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY wg ADD CONSTRAINT wg__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY za ADD CONSTRAINT za__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY zg ADD CONSTRAINT zg__bookmark__key UNIQUE (_bookmark_);
ALTER TABLE ONLY zn ADD CONSTRAINT zn__bookmark__key UNIQUE (_bookmark_);

-- ------------------------------------------------------
SET search_path = extra, pg_catalog;
CREATE INDEX extra_jobbatch_allow_delete ON im_jobbatch USING btree (allow_delete);
-- ------------------------------------------------------
SET search_path = public, pg_catalog;
CREATE INDEX "PlaceALTN" ON pl USING btree (altn);

CREATE UNIQUE INDEX ad_id_key ON ad USING btree (adid);

CREATE INDEX ad_search_index ON ad USING btree (crcd, clid, type, coid, zpcd, stat);
COMMENT ON INDEX ad_search_index IS 'Address search index';

CREATE INDEX adcpy ON ad USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C03'::text, 'S01(Y:N)'::text, 'A50'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (clid)::text, (type)::text, (supf)::text, (cnam)::text, (adid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX adcpy IS 'Addres company key';

CREATE INDEX address_tsv_idx ON ad USING gin (search_phrase);

CREATE INDEX adidk ON ad USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(adid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX adidk IS 'Address unique id';

CREATE INDEX adidkindex ON ad USING btree (adid);

CREATE INDEX adindex ON ad USING btree (adcd, adid, clid, cnam, crcd, hitr, luis, supf, type, zpcd);

CREATE INDEX adkey ON ad USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C03'::text, 'A20'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (clid)::text, (type)::text, (adcd)::text, (adid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX adkey IS 'Courier+Address+Type';

CREATE INDEX adlst ON ad USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C03'::text, 'S01(Y:N)'::text, 'I10'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (clid)::text, (type)::text, (supf)::text, (luis)::text, (adid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX adlst IS 'Addres last used key';

CREATE INDEX adlst_j ON ad USING btree (crcd, clid, type, supf);
COMMENT ON INDEX adlst_j IS 'Addres last used';

CREATE INDEX adpst ON ad USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'A20'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (clid)::text, (zpcd)::text, (adid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX adpst IS 'Address client postcode';

CREATE INDEX adsrh ON ad USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C03'::text, 'S01(Y:N)'::text, 'I08'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (clid)::text, (type)::text, (supf)::text, (hitr)::text, (adid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX adsrh IS 'Address key for usage';

CREATE UNIQUE INDEX an_id_key ON an USING btree (anid);

CREATE INDEX anidk ON an USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(anid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX anidk IS 'Announcement Id Key';

CREATE INDEX anidkindex ON an USING btree (anid);

CREATE INDEX anindex ON an USING btree (anid);

CREATE UNIQUE INDEX ap_id_key ON ap USING btree (apid);

CREATE INDEX apdes ON ap USING btree (norm_key_val(ARRAY['C03'::text, 'A30'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX apdes IS 'Address profile description key';

CREATE INDEX apidk ON ap USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(apid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX apidk IS 'Address format unique key';

CREATE INDEX apidkindex ON ap USING btree (apid);

CREATE INDEX apindex ON ap USING btree ("DESC", apcd, apid, crcd);

CREATE INDEX apkey ON ap USING btree (norm_key_val(ARRAY['C03'::text, 'C20'::text, 'I17'::text], ARRAY[(crcd)::text, (apcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX apkey IS 'Address format key';

CREATE INDEX atindex ON at USING btree (arid, crcd, suid);

CREATE INDEX atkey ON at USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (arid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX atkey IS 'crcd+arid';

CREATE INDEX atsuk ON at USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (arid)::text, (suid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX atsuk IS 'crcd+arid+suid';

CREATE INDEX auevt ON au USING btree (norm_key_val(ARRAY['C03'::text, 'C04'::text, 'A60'::text, 'D11'::text, 'T05'::text, 'I17'::text], ARRAY[(crcd)::text, (file)::text, (recd)::text, (date2text(date))::text, ("time")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX auevt IS 'CRCD+FILE+RECD+DATE+TIME';

CREATE INDEX auindex ON au USING btree (aucd, crcd, date, file, recd, "time", usid);

CREATE INDEX aukey ON au USING btree (norm_key_val(ARRAY['C03'::text, 'C20'::text, 'C12'::text, 'D11'::text, 'T05'::text, 'I17'::text], ARRAY[(crcd)::text, (usid)::text, (aucd)::text, (date2text(date))::text, ("time")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX aukey IS 'Courier+User+Audit Code';

CREATE UNIQUE INDEX bp_id_key ON bp USING btree (bpid);

CREATE INDEX bpdes ON bp USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'A50'::text, 'I17'::text], ARRAY[(crcd)::text, (scrn)::text, (refr)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX bpdes IS ' Booking pref description key';

CREATE INDEX bpidk ON bp USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(bpid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX bpidk IS 'Booking pref unique key';

CREATE INDEX bpidkindex ON bp USING btree (bpid);

CREATE INDEX bpindex ON bp USING btree ("DESC", bpcd, bpid, crcd, prio, refr, scrn);

CREATE INDEX bpkey ON bp USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (scrn)::text, (refr)::text, (bpcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX bpkey IS 'Booking pref key';

CREATE INDEX bpprk ON bp USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'Z03'::text, 'I17'::text], ARRAY[(crcd)::text, (scrn)::text, (refr)::text, (prio)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX bpprk IS 'Booking pref priority key';

CREATE INDEX btadk ON bt USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C12'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (type)::text, (rcid)::text, (tgid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX btadk IS 'Batch Unique Key';

CREATE INDEX btkey ON bt USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (type)::text, (tgid)::text, (rcid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX btkey IS 'Batch key';

CREATE UNIQUE INDEX cd_id_key ON cd USING btree (cdid);

CREATE INDEX cdidk ON cd USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(cdid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cdidk IS 'CD Unique key';

CREATE INDEX cdidkindex ON cd USING btree (cdid);

CREATE INDEX cdindex ON cd USING btree (cdid, clcd, crcd, lscd, sequ, udf1, udf2);

CREATE INDEX cdkey ON cd USING btree (norm_key_val(ARRAY['C03'::text, 'C20'::text, 'C20'::text, 'A50'::text, 'A50'::text, 'A20'::text, 'I17'::text], ARRAY[(crcd)::text, (lscd)::text, (clcd)::text, (udf1)::text, (udf2)::text, (sequ)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cdkey IS 'Client data key';

CREATE INDEX ci_csid_index ON ci USING btree (csid, objt);

CREATE UNIQUE INDEX ci_id_key ON ci USING btree (ciid);

CREATE INDEX ciidk ON ci USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(ciid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX ciidk IS 'Unique id key';

CREATE INDEX ciidkindex ON ci USING btree (ciid);

CREATE INDEX ciindex ON ci USING btree (ciid, crcd, csid, ctyp, lgid, objt, piid, rcnl, seqn, soid, stat, suid);

CREATE INDEX cikey ON ci USING btree (norm_key_val(ARRAY['C03'::text, 'C02'::text, 'C12'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (objt)::text, (csid)::text, (ciid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cikey IS 'Job cost key';

CREATE INDEX cipky ON ci USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'S01(L:D)'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (piid)::text, (stat)::text, (ciid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cipky IS 'Supplier invoice key';

CREATE INDEX cirec ON ci USING btree (norm_key_val(ARRAY['C03'::text, 'S01(L:D)'::text, 'S01(Y:N)'::text, 'C08'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (stat)::text, (rcnl)::text, (suid)::text, (ciid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cirec IS 'Cost line reconcilled key';

CREATE INDEX ciref ON ci USING btree (norm_key_val(ARRAY['C03'::text, 'C02'::text, 'C12'::text, 'C08'::text, 'S01(P:E)'::text, 'C20'::text, 'S01(L:D)'::text, 'I17'::text], ARRAY[(crcd)::text, (objt)::text, (csid)::text, (lgid)::text, (ctyp)::text, (soid)::text, (stat)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX ciref IS 'Cost reference key';

CREATE INDEX ciseq ON ci USING btree (norm_key_val(ARRAY['C03'::text, 'C02'::text, 'C12'::text, 'S01(L:D)'::text, 'Z03'::text, 'I17'::text], ARRAY[(crcd)::text, (objt)::text, (csid)::text, (stat)::text, (seqn)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX ciseq IS 'Cost sequence key';

CREATE INDEX cl_clcd_index ON cl USING btree (clcd);

CREATE INDEX cl_desc_index ON cl USING btree ("DESC");

CREATE UNIQUE INDEX cl_id_key ON cl USING btree (clid);
CREATE INDEX clidkindex ON cl USING btree (clid);

CREATE INDEX cl_sacd_index ON cl USING btree (sacd);

CREATE INDEX cldes ON cl USING btree (norm_key_val(ARRAY['C03'::text, 'A30'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cldes IS 'courier code + client name';

CREATE INDEX clgrb ON cl USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'A30'::text, 'I17'::text], ARRAY[(crcd)::text, (mbcl)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX clgrb IS 'Client booking group key';

CREATE INDEX clgri ON cl USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (micl)::text, (clid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX clgri IS 'Client grour invoice key';

CREATE INDEX clgrp ON cl USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'A30'::text, 'I17'::text], ARRAY[(crcd)::text, (mpcl)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX clgrp IS 'Client group price chart key';

CREATE INDEX clidk ON cl USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(clid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX clidk IS 'Customer Id Key';


CREATE INDEX clindex ON cl USING btree ("DESC", clcd, clid, crcd, mbcl, micl, mpcl);

CREATE INDEX clkey ON cl USING btree (norm_key_val(ARRAY['C03'::text, 'C20'::text, 'I17'::text], ARRAY[(crcd)::text, (clcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX clkey IS 'Courier+Client Code';

CREATE INDEX co_cocd_index ON co USING btree (cocd);

CREATE INDEX co_desc_index ON co USING btree ("DESC");

CREATE UNIQUE INDEX co_id_key ON co USING btree (coid);

CREATE INDEX codes ON co USING btree (norm_key_val(ARRAY['C03'::text, 'A30'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX codes IS 'Country name key';

CREATE INDEX coidk ON co USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(coid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX coidk IS 'Country Id Key';

CREATE INDEX coidkindex ON co USING btree (coid);

CREATE INDEX coindex ON co USING btree ("DESC", cocd, coid, crcd);

CREATE INDEX cokey ON co USING btree (norm_key_val(ARRAY['C03'::text, 'C02'::text, 'I17'::text], ARRAY[(crcd)::text, (cocd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cokey IS 'Country key';

CREATE UNIQUE INDEX cp_id_key ON cp USING btree (cpid);

CREATE INDEX cp_soid_index ON cp USING btree (soid, objt);

CREATE INDEX cpidk ON cp USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(cpid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cpidk IS 'Consignment proporma unique key';

CREATE INDEX cpkey ON cp USING btree (norm_key_val(ARRAY['C03'::text, 'C02'::text, 'C20'::text, 'Z04'::text, 'S01(L:D)'::text, 'I17'::text], ARRAY[(crcd)::text, (objt)::text, (soid)::text, (seqn)::text, (stat)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cpkey IS 'Consignment proporma unique key';

CREATE UNIQUE INDEX cr_id_key ON cr USING btree (crid);

CREATE INDEX cridk ON cr USING btree (norm_key_val(ARRAY['C03'::text, 'I17'::text], ARRAY[(crid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cridk IS 'Courier Information Id Key';

CREATE INDEX cridkindex ON cr USING btree (crid);

CREATE INDEX crindex ON cr USING btree (crcd, crid, pass);

CREATE INDEX crkey ON cr USING btree (norm_key_val(ARRAY['C03'::text, 'I17'::text], ARRAY[(crcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX crkey IS 'Primary Key';

CREATE INDEX crpas ON cr USING btree (norm_key_val(ARRAY['C03'::text, 'I17'::text], ARRAY[(pass)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX crpas IS 'Password key';

CREATE INDEX cs_bkdt_key ON cs USING btree (bkdt);

CREATE UNIQUE INDEX cs_hawb_key ON cs USING btree (hawb);

CREATE UNIQUE INDEX cs_id_key ON cs USING btree (csid);

CREATE INDEX cs_ikey_clid_index ON cs USING btree (ikey, clid, stat);

CREATE INDEX cs_inbn_index ON cs USING btree (inbn);

CREATE INDEX cs_purf_index ON cs USING btree (purf, bkdt, clid, tref, bvia);
CREATE INDEX cs_skey_index ON cs USING btree (skey, bkdt, csid);
CREATE INDEX cs_tref_index ON cs USING btree (tref);
CREATE INDEX csacc ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'S01(Y:N)'::text, 'S01(Y:N)'::text, 'S01(Y:N)'::text, 'D11'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (inok)::text, (riok)::text, (seok)::text, (date2text(bkdt))::text, (csid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX csacc IS 'Job accounts key';
CREATE INDEX csawb ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'A25'::text, 'I17'::text], ARRAY[(crcd)::text, (hawb)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX csawb IS 'Consignment Airway key';
CREATE INDEX cscky ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'A10'::text, 'S01(Y:N)'::text, 'D11'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (bmet)::text, (chng)::text, (date2text(bkdt))::text, (csid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cscky IS 'Booking method key';
CREATE INDEX csclk ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'D11'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (clid)::text, (date2text(bkdt))::text, (rjid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX csclk IS 'Consignment client key';
CREATE INDEX csdat ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'D11'::text, 'S01(P:Q:L:H)'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (date2text(bkdt))::text, (stat)::text, (rjid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX csdat IS 'Job booking date key';
CREATE INDEX csfky ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (fcid)::text, (fpid)::text, (csid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX csfky IS 'Pickup country+place+job';
CREATE INDEX csidk ON cs USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(csid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX csidk IS 'Unique id key';
CREATE INDEX csindex ON cs USING btree (bkdt, bkok, bmet, chng, clid, crcd, cref, crf2, csid, dlok, dvok, fcid, fpid, hawb, inbn, inok, kpi1, pdid, podt, riok, rjid, seok, skey, stat, tcid, tpid, tref, ud11);
CREATE INDEX csinv ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'A20'::text, 'S01(Y:N)'::text, 'S01(Y:N)'::text, 'S01(P:Q:L:H)'::text, 'A25'::text, 'I17'::text], ARRAY[(crcd)::text, (inbn)::text, (bkok)::text, (riok)::text, (stat)::text, (hawb)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX csinv IS 'Consignment invoice key';
CREATE INDEX cskey ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'A60'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (ud11)::text, (csid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cskey IS 'CRCD+CSID';
CREATE INDEX cskpi ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'A75'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (kpi1)::text, (rjid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cskpi IS 'KPI key';
CREATE INDEX cspdk ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (pdid)::text, (rjid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cspdk IS 'Job product key';
CREATE INDEX csprs ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'S01(Y:N)'::text, 'S01(Y:N)'::text, 'S01(Y:N)'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (bkok)::text, (dvok)::text, (dlok)::text, (csid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX csprs IS 'Consignment processoking key';
CREATE INDEX csref ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'A90'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (cref)::text, (rjid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX csref IS 'Consignment ref key';
CREATE INDEX csrf2 ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'A90'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (crf2)::text, (rjid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX csrf2 IS 'Job second reference key';
CREATE INDEX cssky ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'C20'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (skey)::text, (rjid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cssky IS 'Job search key';
CREATE INDEX cssts ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'C20'::text, 'D11'::text, 'D11'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (skey)::text, (date2text(podt))::text, (date2text(bkdt))::text, (rjid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cssts IS 'Consignment status key';
CREATE INDEX cstky ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (tcid)::text, (tpid)::text, (csid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cstky IS 'Destination country+place+job';
CREATE INDEX cstrf ON cs USING btree (norm_key_val(ARRAY['C03'::text, 'A25'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (tref)::text, (rjid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cstrf IS 'Job third party ref';
CREATE UNIQUE INDEX ct_id_key ON ct USING btree ("CTID");
CREATE INDEX ct_usnm_index ON ct USING btree (usnm);
CREATE INDEX ctdes ON ct USING btree (norm_key_val(ARRAY['C03'::text, 'A40'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX ctdes IS 'Contact description (name) key';
CREATE INDEX ctidk ON ct USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[("CTID")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX ctidk IS 'Contact unique key';
CREATE INDEX ctidkindex ON ct USING btree ("CTID");
CREATE INDEX ctindex ON ct USING btree ("CTID", "DESC", crcd, lnam, refr, sequ, type, usnm);
CREATE INDEX ctlnm ON ct USING btree (norm_key_val(ARRAY['C03'::text, 'A20'::text, 'I17'::text], ARRAY[(crcd)::text, (lnam)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX ctlnm IS 'Contact last name';
CREATE INDEX cttyp ON ct USING btree (norm_key_val(ARRAY['C03'::text, 'C12'::text, 'C03'::text, 'A40'::text, 'I01'::text, 'I17'::text], ARRAY[(crcd)::text, (refr)::text, (type)::text, ("DESC")::text, (sequ)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cttyp IS 'Contact type key';
CREATE INDEX ctusk ON ct USING btree (norm_key_val(ARRAY['C03'::text, 'C12'::text, 'C03'::text, 'A20'::text, 'I17'::text], ARRAY[(crcd)::text, (refr)::text, (type)::text, (usnm)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX ctusk IS 'Contact user name key';
CREATE INDEX cu_cucd_index ON cu USING btree (cucd);
CREATE UNIQUE INDEX cu_id_key ON cu USING btree (cuid);
CREATE INDEX cudes ON cu USING btree (norm_key_val(ARRAY['C03'::text, 'A30'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cudes IS 'CRCD+DESC';
CREATE INDEX cuidk ON cu USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(cuid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cuidk IS 'Currency Id Key';
CREATE INDEX cuidkindex ON cu USING btree (cuid);
CREATE INDEX cuindex ON cu USING btree ("DESC", crcd, cucd, cuid);
CREATE INDEX cukey ON cu USING btree (norm_key_val(ARRAY['C03'::text, 'C03'::text, 'I17'::text], ARRAY[(crcd)::text, (cucd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cukey IS 'Courier+Currency Code';
CREATE UNIQUE INDEX cv_id_key ON cv USING btree (csid);
CREATE INDEX cvidk ON cv USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(csid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cvidk IS 'Virtual key';
CREATE INDEX cvidkindex ON cv USING btree (csid);
CREATE INDEX cvindex ON cv USING btree (csid);
CREATE UNIQUE INDEX cy_id_key ON cy USING btree (cyid);
CREATE INDEX cyend ON cy USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'D11'::text, 'I17'::text], ARRAY[(crcd)::text, (cuid)::text, (date2text(edat))::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cyend IS 'Currency end date key';
CREATE INDEX cyidk ON cy USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(cyid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cyidk IS 'Currency rate unique is';
CREATE INDEX cyidkindex ON cy USING btree (cyid);
CREATE INDEX cyindex ON cy USING btree (crcd, cuid, cyid, edat, sdat);
CREATE INDEX cykey ON cy USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'D11'::text, 'I17'::text], ARRAY[(crcd)::text, (cuid)::text, (date2text(sdat))::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cykey IS 'Courier+Currency+Start Date';
CREATE INDEX cyky ON cy USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'D11'::text, 'D11'::text, 'I17'::text], ARRAY[(crcd)::text, (cuid)::text, (date2text(sdat))::text, (date2text(edat))::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX cyky IS 'Courier+Currency+SDAT+EDAT';
CREATE INDEX dc_csid_index ON dc USING btree (dccd);
CREATE UNIQUE INDEX dc_id_key ON dc USING btree (dcid);
CREATE INDEX dcidk ON dc USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(dcid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX dcidk IS 'Unique id key';
CREATE INDEX dckey ON dc USING btree (norm_key_val(ARRAY['C03'::text, 'C02'::text, 'C12'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (objt)::text, (dccd)::text, (dcid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX dckey IS 'Document key';
CREATE INDEX di_id_key ON di USING btree (diid);
CREATE INDEX diidk ON di USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(diid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX diidk IS 'Debug Id Key';
CREATE INDEX diidkindex ON di USING btree (diid);
CREATE INDEX dikey ON di USING btree (norm_key_val(ARRAY['C03'::text, 'D11'::text, 'Z02'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (date2text(date))::text, (levl)::text, (diid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX dikey IS 'Debug info key';
CREATE INDEX dipky ON di USING btree (norm_key_val(ARRAY['C03'::text, 'A20'::text, 'D11'::text, 'Z02'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (prgm)::text, (date2text(date))::text, (levl)::text, (diid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX dipky IS 'Debug info key by program';
CREATE UNIQUE INDEX dt_id_key ON dt USING btree (dtid);
CREATE INDEX dt_rtid_index ON dt USING btree (rtid);
CREATE INDEX dtdes ON dt USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'A10'::text, 'I17'::text], ARRAY[(crcd)::text, (rtid)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX dtdes IS 'CRCD+RTID+DESC';
CREATE INDEX dtidk ON dt USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(dtid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX dtidk IS 'Unique id key';
CREATE INDEX dtidkindex ON dt USING btree (dtid);
CREATE INDEX dtindex ON dt USING btree ("DESC", crcd, dtcd, dtid, rtid);
CREATE INDEX dtkey ON dt USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C03'::text, 'I17'::text], ARRAY[(crcd)::text, (rtid)::text, (dtcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX dtkey IS 'CRCD+RTID+DTCD';
CREATE INDEX ea_exid_index ON ea USING btree (exid);
CREATE UNIQUE INDEX ea_id_key ON ea USING btree (eaid);
CREATE INDEX eaext ON ea USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'N8.4'::text, 'Z02'::text, 'I17'::text], ARRAY[(crcd)::text, (exid)::text, (fzid)::text, (tzid)::text, (refr)::text, (fbnd)::text, (prio)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX eaext IS 'CRCD+EXID+FZID+TZID+FBND+REFR';
CREATE INDEX eaidk ON ea USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(eaid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX eaidk IS 'Extra allocation unique key';
CREATE INDEX eaidkindex ON ea USING btree (eaid);
CREATE INDEX eaindex ON ea USING btree (crcd, eaid, exid, fbnd, fzid, prio, refr, tzid);
CREATE INDEX eakey ON ea USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (fzid)::text, (tzid)::text, (refr)::text, (exid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX eakey IS 'Courier extra allocation key';
CREATE INDEX eaprk ON ea USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'Z02'::text, 'I17'::text], ARRAY[(crcd)::text, (exid)::text, (prio)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX eaprk IS 'Courier extra allocation prio key';
CREATE INDEX earer ON ea USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (refr)::text, (fzid)::text, (tzid)::text, (exid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX earer IS 'Courier extra alloc for client supplier ';
CREATE INDEX ex_excd_index ON ex USING btree (excd, meth);
CREATE UNIQUE INDEX ex_id_key ON ex USING btree (exid);
CREATE INDEX exidk ON ex USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(exid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX exidk IS 'Extra unique Key';
CREATE INDEX exidkindex ON ex USING btree (exid);
CREATE INDEX exindex ON ex USING btree (crcd, excd, exid, meth, prio, type);
CREATE INDEX exkey ON ex USING btree (norm_key_val(ARRAY['C03'::text, 'A25'::text, 'S01(S:C)'::text, 'I17'::text], ARRAY[(crcd)::text, (excd)::text, (type)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX exkey IS 'Courier extra Key';
CREATE INDEX exmth ON ex USING btree (norm_key_val(ARRAY['C03'::text, 'C03'::text, 'S01(S:C)'::text, 'Z02'::text, 'I17'::text], ARRAY[(crcd)::text, (meth)::text, (type)::text, (prio)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX exmth IS 'Courier extra method';
CREATE INDEX exprk ON ex USING btree (norm_key_val(ARRAY['C03'::text, 'Z02'::text, 'A25'::text, 'I17'::text], ARRAY[(crcd)::text, (prio)::text, (excd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX exprk IS 'Courier extra priority Key';
CREATE INDEX extyp ON ex USING btree (norm_key_val(ARRAY['C03'::text, 'S01(S:C)'::text, 'Z02'::text, 'I17'::text], ARRAY[(crcd)::text, (type)::text, (prio)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX extyp IS 'Courier extra type Key';
CREATE INDEX ho_fdat_index ON ho USING btree (fdat);
CREATE UNIQUE INDEX ho_id_key ON ho USING btree (hoid);
CREATE INDEX hoidk ON ho USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(hoid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX hoidk IS 'Holiday unique id key';
CREATE INDEX hoidkindex ON ho USING btree (hoid);
CREATE INDEX hoindex ON ho USING btree (coid, crcd, fdat, hoid);
CREATE INDEX hokey ON ho USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'D11'::text, 'I17'::text], ARRAY[(crcd)::text, (coid)::text, (date2text(fdat))::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX hokey IS 'Courier+Country';
CREATE UNIQUE INDEX ip_id_key ON ip USING btree (ipid);
CREATE INDEX ipidk ON ip USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(ipid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX ipidk IS 'Invoice payment id key';
CREATE INDEX ipidkindex ON ip USING btree (ipid);
CREATE INDEX ipindex ON ip USING btree (crcd, ipid, pmid, siid);
CREATE INDEX ipink ON ip USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (siid)::text, (pmid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX ipink IS 'Payment invoice key';
CREATE INDEX ipkey ON ip USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (pmid)::text, (siid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX ipkey IS 'Payment key';
CREATE UNIQUE INDEX it_id_key ON it USING btree (itid);
CREATE INDEX it_itcd_index ON it USING btree (itcd, clid);
CREATE INDEX itcky ON it USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C20'::text, 'I17'::text], ARRAY[(crcd)::text, (clid)::text, (itcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX itcky IS 'Item client key';
CREATE INDEX itdes ON it USING btree (norm_key_val(ARRAY['C03'::text, 'A50'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX itdes IS 'Item description key';
CREATE INDEX itidk ON it USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(itid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX itidk IS 'Item unique key';
CREATE INDEX itidkindex ON it USING btree (itid);
CREATE INDEX itindex ON it USING btree ("DESC", clid, crcd, itcd, itid);
CREATE INDEX itkey ON it USING btree (norm_key_val(ARRAY['C03'::text, 'C20'::text, 'I17'::text], ARRAY[(crcd)::text, (itcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX itkey IS 'Item key';
CREATE INDEX ja_csid_index ON ja USING btree (csid);
CREATE UNIQUE INDEX ja_id_key ON ja USING btree (jaid);
CREATE INDEX jaidk ON ja USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(jaid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX jaidk IS 'Unique id key';
CREATE INDEX jaidkindex ON ja USING btree (jaid);
CREATE INDEX jaindex ON ja USING btree (crcd, csid, jaid);
CREATE INDEX jakey ON ja USING btree (norm_key_val(ARRAY['C03'::text, 'C12'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (csid)::text, (jaid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX jakey IS 'courier+job+job alert';
CREATE INDEX jl_csid_key ON jl USING btree (csid);
CREATE UNIQUE INDEX jl_id_key ON jl USING btree (jlid);
CREATE INDEX jlbag ON jl USING btree (norm_key_val(ARRAY['C03'::text, 'C02'::text, 'C08'::text, 'A30'::text, 'S01(L:D)'::text, 'I17'::text], ARRAY[(crcd)::text, (objt)::text, (suid)::text, (bagn)::text, (stat)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX jlbag IS 'Job leg Supplier ref';
CREATE INDEX jlidk ON jl USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(jlid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX jlidk IS 'Unique id key';
CREATE INDEX jlidkindex ON jl USING btree (jlid);
CREATE INDEX jlindex ON jl USING btree (bagn, crcd, csid, jlid, lgid, mfid, objt, pbdt, seqn, stat, suid);
CREATE INDEX jlkey ON jl USING btree (norm_key_val(ARRAY['C03'::text, 'C02'::text, 'C12'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (objt)::text, (csid)::text, (lgid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX jlkey IS 'Job leg key';
CREATE INDEX jlmfk ON jl USING btree (norm_key_val(ARRAY['C03'::text, 'C12'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (mfid)::text, (jlid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX jlmfk IS 'Job leg manifest key';
CREATE INDEX jlref ON jl USING btree (norm_key_val(ARRAY['C03'::text, 'C02'::text, 'C08'::text, 'D11'::text, 'S01(L:D)'::text, 'I17'::text], ARRAY[(crcd)::text, (objt)::text, (suid)::text, (date2text(pbdt))::text, (stat)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX jlref IS 'Job leg Supplier ref';
CREATE INDEX jlseq ON jl USING btree (norm_key_val(ARRAY['C03'::text, 'C02'::text, 'C12'::text, 'S01(L:D)'::text, 'Z04'::text, 'I17'::text], ARRAY[(crcd)::text, (objt)::text, (csid)::text, (stat)::text, (seqn)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX jlseq IS 'Job leg sequence key';
CREATE INDEX jlsta ON jl USING btree (norm_key_val(ARRAY['C03'::text, 'C02'::text, 'C12'::text, 'C08'::text, 'S01(L:D)'::text, 'I17'::text], ARRAY[(crcd)::text, (objt)::text, (csid)::text, (lgid)::text, (stat)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX jlsta IS 'Job leg status';
CREATE INDEX jp_csid_index ON jp USING btree (csid, objt);
CREATE UNIQUE INDEX jp_id_key ON jp USING btree (jpid);
CREATE INDEX jpidk ON jp USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(jpid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX jpidk IS 'Unique id key';
CREATE INDEX jpidkindex ON jp USING btree (jpid);
CREATE INDEX jpindex ON jp USING btree (crcd, csid, jpid, objt, seqn, soid, stat, type);
CREATE INDEX jpkey ON jp USING btree (norm_key_val(ARRAY['C03'::text, 'C02'::text, 'C12'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (objt)::text, (csid)::text, (jpid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX jpkey IS 'CRCD+OBJT+CSID+JPID';
CREATE INDEX jpseq ON jp USING btree (norm_key_val(ARRAY['C03'::text, 'C02'::text, 'C12'::text, 'S01(L:D)'::text, 'Z04'::text, 'I17'::text], ARRAY[(crcd)::text, (objt)::text, (csid)::text, (stat)::text, (seqn)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX jpseq IS 'Job price sequence key';
CREATE INDEX jptyp ON jp USING btree (norm_key_val(ARRAY['C03'::text, 'C02'::text, 'C12'::text, 'S01(P:D:E)'::text, 'C20'::text, 'S01(L:D)'::text, 'I17'::text], ARRAY[(crcd)::text, (objt)::text, (csid)::text, (type)::text, (soid)::text, (stat)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX jptyp IS 'Job type source key';
CREATE INDEX key_1 ON syst USING btree (norm_key_val(ARRAY['C06'::text, 'I17'::text], ARRAY[(name)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
CREATE INDEX key_10 ON reln USING btree (norm_key_val(ARRAY['C06'::text, 'I17'::text], ARRAY[(filfrm)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
CREATE INDEX key_11 ON helps USING btree (norm_key_val(ARRAY['C06'::text, 'I17'::text], ARRAY[(name)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
CREATE INDEX key_12 ON proc USING btree (norm_key_val(ARRAY['A06'::text, 'I17'::text], ARRAY[(name)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
CREATE INDEX key_13 ON menu USING btree (norm_key_val(ARRAY['I02'::text, 'I01'::text, 'I17'::text], ARRAY[(levl)::text, (optn)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
CREATE INDEX key_14 ON fields USING btree (norm_key_val(ARRAY['I05'::text, 'I05'::text, 'I17'::text], ARRAY[(fnum)::text, (dnum)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
CREATE INDEX key_2 ON phys USING btree (norm_key_val(ARRAY['C06'::text, 'I17'::text], ARRAY[(pname)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
CREATE INDEX key_3 ON files USING btree (norm_key_val(ARRAY['C06'::text, 'I17'::text], ARRAY[(name)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
CREATE INDEX key_4 ON gflds USING btree (norm_key_val(ARRAY['C06'::text, 'I17'::text], ARRAY[(name)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
CREATE INDEX key_5 ON fields USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(name)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
CREATE INDEX key_8 ON keys USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(name)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
CREATE INDEX key_9 ON keys USING btree (norm_key_val(ARRAY['I05'::text, 'I05'::text, 'I17'::text], ARRAY[(fnum)::text, (num)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
CREATE UNIQUE INDEX lf_id_key ON lf USING btree (lfid);
CREATE INDEX lfidk ON lf USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(lfid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX lfidk IS 'LFID';
CREATE INDEX lfidkindex ON lf USING btree (lfid);
CREATE INDEX lfindex ON lf USING btree (crcd, lfcd, lfid, seqn, type);
CREATE INDEX lfkey ON lf USING btree (norm_key_val(ARRAY['C03'::text, 'C03'::text, 'C08'::text, 'Z05'::text, 'I17'::text], ARRAY[(crcd)::text, (type)::text, (lfcd)::text, (seqn)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX lfkey IS 'CRCD+TYPE+LFCD+SEQN';
CREATE UNIQUE INDEX lg_id_key ON lg USING btree (lgid);
CREATE INDEX lg_lgcd_index ON lg USING btree (lgcd);
CREATE INDEX lgidk ON lg USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(lgid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX lgidk IS 'Leg ID';
CREATE INDEX lgidkindex ON lg USING btree (lgid);
CREATE INDEX lgindex ON lg USING btree (crcd, fpid, fzid, lgcd, lgid, prio, suid, tpid, type, tzid);
CREATE INDEX lgkey ON lg USING btree (norm_key_val(ARRAY['C03'::text, 'A50'::text, 'I17'::text], ARRAY[(crcd)::text, (lgcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX lgkey IS 'Leg key';
CREATE INDEX lgplk ON lg USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'A50'::text, 'I17'::text], ARRAY[(crcd)::text, (fpid)::text, (tpid)::text, (type)::text, (lgcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX lgplk IS 'Leg place key';
CREATE INDEX lgsup ON lg USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'Z02'::text, 'I17'::text], ARRAY[(crcd)::text, (suid)::text, (fpid)::text, (tzid)::text, (prio)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX lgsup IS 'Leg supplier key';
CREATE INDEX lgval ON lg USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'Z02'::text, 'A50'::text, 'I17'::text], ARRAY[(crcd)::text, (fzid)::text, (type)::text, (prio)::text, (lgcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX lgval IS 'Leg from zone key';
CREATE UNIQUE INDEX lh_id_key ON lh USING btree (lhid);
CREATE INDEX lhdes ON lh USING btree (norm_key_val(ARRAY['C03'::text, 'A30'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX lhdes IS 'List description key';
CREATE INDEX lhidk ON lh USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(lhid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX lhidk IS 'List unique key';
CREATE INDEX lhidkindex ON lh USING btree (lhid);
CREATE INDEX lhindex ON lh USING btree ("DESC", crcd, lhcd, lhid);
CREATE INDEX lhkey ON lh USING btree (norm_key_val(ARRAY['C03'::text, 'C20'::text, 'I17'::text], ARRAY[(crcd)::text, (lhcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX lhkey IS 'List prefix key';
CREATE UNIQUE INDEX lo_id_key ON lo USING btree (loid);
CREATE INDEX lo_locd_index ON lo USING btree (locd);
CREATE INDEX loidk ON lo USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(loid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX loidk IS 'Location unique key';
CREATE INDEX loidkindex ON lo USING btree (loid);
CREATE INDEX lokey ON lo USING btree (norm_key_val(ARRAY['C03'::text, 'C20'::text, 'I17'::text], ARRAY[(crcd)::text, (locd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX lokey IS 'Location key';
CREATE INDEX loseq ON lo USING btree (norm_key_val(ARRAY['C03'::text, 'I08'::text, 'C20'::text, 'I17'::text], ARRAY[(crcd)::text, (pseq)::text, (locd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX loseq IS 'Location key';
CREATE UNIQUE INDEX ls_id_key ON ls USING btree (lsid);
CREATE INDEX ls_lsvl_seqn_index ON ls USING btree (lspx, lsvl, seqn);
CREATE INDEX ls_seqn_index ON ls USING btree (lspx, seqn);
CREATE INDEX lsidk ON ls USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(lsid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX lsidk IS 'List File Unique Key';
CREATE INDEX lsidkindex ON ls USING btree (lsid);
CREATE INDEX lsiky ON ls USING btree (norm_key_val(ARRAY['C03'::text, 'C20'::text, 'A50'::text, 'I17'::text], ARRAY[(crcd)::text, (lspx)::text, (note)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX lsiky IS 'CRCD+LSPX+Note';
CREATE INDEX lsindex ON ls USING btree (crcd, lsid, lspx, lsvl, note, seqn);
CREATE INDEX lskey ON ls USING btree (norm_key_val(ARRAY['C03'::text, 'C20'::text, 'A200'::text, 'I17'::text], ARRAY[(crcd)::text, (lspx)::text, (lsvl)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX lskey IS 'List File external key';
CREATE INDEX lsseq ON ls USING btree (norm_key_val(ARRAY['C03'::text, 'C20'::text, 'A200'::text, 'A200'::text, 'A50'::text, 'I17'::text], ARRAY[(crcd)::text, (lspx)::text, (seqn)::text, (lsvl)::text, (note)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX lsseq IS 'CRCD+LSPX+SEQN';
CREATE UNIQUE INDEX mf_id_key ON mf USING btree (mfid);
CREATE INDEX mfcdt ON mf USING btree (norm_key_val(ARRAY['C03'::text, 'D11'::text, 'T05'::text, 'I17'::text], ARRAY[(crcd)::text, (date2text(cldt))::text, (cltm)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX mfcdt IS 'Manifets close date';
CREATE INDEX mfdat ON mf USING btree (norm_key_val(ARRAY['C03'::text, 'D11'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (date2text(mfdt))::text, (mfid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX mfdat IS 'Manifest date key';
CREATE INDEX mfidk ON mf USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(mfid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX mfidk IS 'Unique id key';
CREATE INDEX mfidkindex ON mf USING btree (mfid);
CREATE INDEX mfindex ON mf USING btree (cldt, cltm, crcd, lgid, mfcd, mfdt, mfid, open, refr, stat, suid);
CREATE INDEX mfkey ON mf USING btree (norm_key_val(ARRAY['C03'::text, 'A20'::text, 'I17'::text], ARRAY[(crcd)::text, (mfcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX mfkey IS 'Manifest key(CRCD+MFCD)';
CREATE INDEX mflky ON mf USING btree (norm_key_val(ARRAY['C03'::text, 'S01(O:C:H)'::text, 'C08'::text, 'S01(L:D)'::text, 'I17'::text], ARRAY[(crcd)::text, (open)::text, (lgid)::text, (stat)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX mflky IS 'Manifest leg key';
CREATE INDEX mfref ON mf USING btree (norm_key_val(ARRAY['C03'::text, 'S01(O:C:H)'::text, 'A20'::text, 'S01(L:D)'::text, 'I17'::text], ARRAY[(crcd)::text, (open)::text, (refr)::text, (stat)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX mfref IS 'Manifest reference key';
CREATE INDEX mfsup ON mf USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'S01(O:C:H)'::text, 'D11'::text, 'I17'::text], ARRAY[(crcd)::text, (suid)::text, (open)::text, (date2text(mfdt))::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX mfsup IS 'Manifest supplier';
CREATE INDEX mqindex ON mq USING btree (crcd, flag, keyn, keyv, mqid);
CREATE INDEX mqkey ON mq USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'A100'::text, 'I17'::text], ARRAY[(crcd)::text, (keyn)::text, (keyv)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX mqkey IS 'Mirror reference key';
CREATE INDEX mqque ON mq USING btree (norm_key_val(ARRAY['C03'::text, 'S01(Y:N)'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (flag)::text, (mqid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX mqque IS 'Mirror queue key';
CREATE INDEX nsindex ON ns USING btree (opid, refr, type);
CREATE INDEX nskey ON ns USING btree (norm_key_val(ARRAY['S01(U:G:O:P)'::text, 'C04'::text, 'C03'::text, 'I17'::text], ARRAY[(type)::text, (refr)::text, (opid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX nskey IS 'Number Series Key';
CREATE UNIQUE INDEX nt_id_key ON nt USING btree (ntid);
CREATE INDEX ntidk ON nt USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(ntid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX ntidk IS 'Note unique key';
CREATE INDEX ntidkindex ON nt USING btree (ntid);
CREATE INDEX ntindex ON nt USING btree (crcd, doby, dotm, dscs, ntcd, ntid, pobj, rmdt, rmtm, type);
CREATE INDEX ntjob ON nt USING btree (norm_key_val(ARRAY['C03'::text, 'S01(Y:N)'::text, 'C06'::text, 'C80'::text, 'I17'::text], ARRAY[(crcd)::text, (dscs)::text, (pobj)::text, (ntcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX ntjob IS 'Note job show key';
CREATE INDEX ntkey ON nt USING btree (norm_key_val(ARRAY['C03'::text, 'S01(N:T:A)'::text, 'D11'::text, 'T05'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (type)::text, (date2text(doby))::text, (dotm)::text, (ntid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX ntkey IS 'Note key';
CREATE INDEX ntky ON nt USING btree (norm_key_val(ARRAY['C03'::text, 'S01(N:T:A)'::text, 'C06'::text, 'C80'::text, 'D11'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (type)::text, (pobj)::text, (ntcd)::text, (date2text(doby))::text, (ntid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX ntky IS 'Note type search';
CREATE INDEX nttyp ON nt USING btree (norm_key_val(ARRAY['C03'::text, 'S01(N:T:A)'::text, 'D11'::text, 'T05'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (type)::text, (date2text(rmdt))::text, (rmtm)::text, (ntid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX nttyp IS 'Note type key';
CREATE UNIQUE INDEX pa_id_key ON pa USING btree (paid);
CREATE INDEX pa_pcid_index ON pa USING btree (pcid);
CREATE INDEX pa_zones_index ON pa USING btree (fzid, tzid, clid);
CREATE INDEX paclk ON pa USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'Z02'::text, 'I17'::text], ARRAY[(crcd)::text, (clid)::text, (fzid)::text, (tzid)::text, (prio)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX paclk IS 'Price chart client Key';
CREATE INDEX paidk ON pa USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(paid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX paidk IS 'Price allocation unique key';
CREATE INDEX paindex ON pa USING btree (clid, crcd, ctyp, fzid, paid, pcid, pdid, prio, slid, tzid);
CREATE INDEX pakey ON pa USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (pdid)::text, (slid)::text, (ctyp)::text, (clid)::text, (fzid)::text, (tzid)::text, (pcid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pakey IS 'Price allocation key';
CREATE INDEX papky ON pa USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (pcid)::text, (paid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX papky IS 'Price allocation by price chart';
CREATE INDEX pasrh ON pa USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'Z02'::text, 'I17'::text], ARRAY[(crcd)::text, (pdid)::text, (fzid)::text, (tzid)::text, (prio)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pasrh IS 'Price search key';
CREATE INDEX paznk ON pa USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'Z02'::text, 'I17'::text], ARRAY[(crcd)::text, (fzid)::text, (tzid)::text, (prio)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX paznk IS 'Price chart zone Key';
CREATE UNIQUE INDEX pc_id_key ON pc USING btree (pcid);
CREATE INDEX pc_pccd_index ON pc USING btree (pccd);
CREATE INDEX pcdes ON pc USING btree (norm_key_val(ARRAY['C03'::text, 'A75'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pcdes IS 'Courier+Description';
CREATE INDEX pcidk ON pc USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(pcid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pcidk IS 'Price chart unique Key';
CREATE INDEX pcidkindex ON pc USING btree (pcid);
CREATE INDEX pcindex ON pc USING btree ("DESC", crcd, pccd, pcid);
CREATE INDEX pckey ON pc USING btree (norm_key_val(ARRAY['C03'::text, 'A75'::text, 'I17'::text], ARRAY[(crcd)::text, (pccd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pckey IS 'Courier+Price chart code key';
CREATE UNIQUE INDEX pd_id_key ON pd USING btree (pdid);
CREATE INDEX pddes ON pd USING btree (norm_key_val(ARRAY['C03'::text, 'A50'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pddes IS 'Courier+Description Key';
CREATE INDEX pdidk ON pd USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(pdid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pdidk IS 'Product primary key';
CREATE INDEX pdidkindex ON pd USING btree (pdid);
CREATE INDEX pdindex ON pd USING btree ("DESC", crcd, pdcd, pdid);
CREATE INDEX pdkey ON pd USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (pdcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pdkey IS 'Courier+Product key';
CREATE UNIQUE INDEX pe_id_key ON pe USING btree (peid);
CREATE INDEX peidk ON pe USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(peid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX peidk IS 'Potential extra Id Key';
CREATE INDEX peidkindex ON pe USING btree (peid);
CREATE INDEX peindex ON pe USING btree (crcd, exid, paid, peid);
CREATE INDEX pekey ON pe USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (paid)::text, (exid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pekey IS 'Price allocation extra key';
CREATE UNIQUE INDEX pi_id_key ON pi USING btree (piid);
CREATE INDEX pi_sivn_index ON pi USING btree (sivn);
CREATE INDEX piclo ON pi USING btree (norm_key_val(ARRAY['C03'::text, 'S01(Y:N)'::text, 'C08'::text, 'A20'::text, 'I17'::text], ARRAY[(crcd)::text, (clos)::text, (suid)::text, (sivn)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX piclo IS 'Purchase invoice close key';
CREATE INDEX piidk ON pi USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(piid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX piidk IS 'Purchase Invoice Id Key';
CREATE INDEX piidkindex ON pi USING btree (piid);
CREATE INDEX piindex ON pi USING btree (clos, crcd, piid, sivn, suid);
CREATE INDEX piinv ON pi USING btree (norm_key_val(ARRAY['C03'::text, 'A20'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (sivn)::text, (suid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX piinv IS 'Courier+Supplier Inv.+Supplier';
CREATE INDEX pikey ON pi USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'A20'::text, 'I17'::text], ARRAY[(crcd)::text, (suid)::text, (sivn)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pikey IS 'Courier+Supplier Inv.No';
CREATE INDEX pl_altn_index ON pl USING btree (altn);
CREATE INDEX pl_desc_index ON pl USING btree ("DESC");
CREATE UNIQUE INDEX pl_id_key ON pl USING btree (plid);
CREATE INDEX pl_lower_altn_idx ON pl USING btree (coid, lower((altn)::text));
CREATE INDEX pl_lower_desc_idx ON pl USING btree (coid, lower(("DESC")::text));
CREATE INDEX pl_plfr_altn_lpad_idx ON pl USING btree (psfr_lpad, altn_lpad);
CREATE INDEX place_tsv_idx ON pl USING gin (search_phrase);
CREATE INDEX plalt ON pl USING btree (norm_key_val(ARRAY['C03'::text, 'A50'::text, 'I17'::text], ARRAY[(crcd)::text, (altn)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX plalt IS 'Alternative name key';
CREATE INDEX plcal ON pl USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'A50'::text, 'I17'::text], ARRAY[(crcd)::text, (coid)::text, (altn)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX plcal IS 'Place Alternative key';
CREATE INDEX plcky ON pl USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'A50'::text, 'I17'::text], ARRAY[(crcd)::text, (coid)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX plcky IS 'Place country key';
CREATE INDEX pldes ON pl USING btree (norm_key_val(ARRAY['C03'::text, 'A50'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pldes IS 'Place description';
CREATE INDEX plidk ON pl USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(plid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX plidk IS 'Place unique id key';
CREATE INDEX plidkindex ON pl USING btree (plid);
CREATE INDEX plindex ON pl USING btree ("DESC", altn, coid, crcd, plid, type);
CREATE INDEX pltyp ON pl USING btree (norm_key_val(ARRAY['C03'::text, 'S02(PO:OD:CT:PC:SR:OT:NT:AL)'::text, 'I17'::text], ARRAY[(crcd)::text, (type)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pltyp IS 'Place type key';
CREATE UNIQUE INDEX pm_id_key ON pm USING btree (pmid);
CREATE INDEX pmclk ON pm USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'D11'::text, 'I17'::text], ARRAY[(crcd)::text, (clid)::text, (date2text(rcdt))::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pmclk IS 'Payment client key';
CREATE INDEX pmidk ON pm USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(pmid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pmidk IS 'Payment id key';
CREATE INDEX pmidkindex ON pm USING btree (pmid);
CREATE INDEX pmindex ON pm USING btree (clid, crcd, pmcd, pmid, rcdt);
CREATE INDEX pmkey ON pm USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'A30'::text, 'I17'::text], ARRAY[(crcd)::text, (clid)::text, (pmcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pmkey IS 'Payment key';
CREATE UNIQUE INDEX pr_id_key ON pr USING btree (prid);
CREATE INDEX pr_pcid_index ON pr USING btree (pcid);
CREATE INDEX pridk ON pr USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(prid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX pridk IS 'Price line unique key';
CREATE INDEX pridkindex ON pr USING btree (prid);
CREATE INDEX prindex ON pr USING btree (crcd, pcid, prid, stwt);
CREATE INDEX prkey ON pr USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (pcid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX prkey IS 'Courier+PriceChart Code';
CREATE INDEX prsky ON pr USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'N8.2'::text, 'I17'::text], ARRAY[(crcd)::text, (pcid)::text, (stwt)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX prsky IS 'CRCD+PCID+STWT';
CREATE INDEX rdindex ON rd USING btree ("FROM", "TO");
CREATE INDEX rdkey ON rd USING btree (norm_key_val(ARRAY['C15'::text, 'C02'::text, 'I17'::text], ARRAY[("FROM")::text, ("TO")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX rdkey IS 'FROM+TO';
CREATE INDEX rdky ON rd USING btree (norm_key_val(ARRAY['C15'::text, 'I17'::text], ARRAY[("FROM")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX rdky IS 'CRCD+FROM';
CREATE UNIQUE INDEX re_id_key ON re USING btree (reid);
CREATE INDEX reidk ON re USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(reid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX reidk IS 'Unique id key';
CREATE INDEX rekey ON re USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (rtid)::text, (exid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX rekey IS 'CRCD+RTID+EXID';
CREATE INDEX reseq ON re USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'I03'::text, 'I17'::text], ARRAY[(crcd)::text, (rtid)::text, (sequ)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX reseq IS 'CRCD+RTID+SEQU';
CREATE UNIQUE INDEX rl_id_key ON rl USING btree (rlid);
CREATE INDEX rl_rtid_index ON rl USING btree (rtid);
CREATE INDEX rlidk ON rl USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(rlid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX rlidk IS 'Unique id key';
CREATE INDEX rlidkindex ON rl USING btree (rlid);
CREATE INDEX rlindex ON rl USING btree (crcd, lgid, rlid, rtid, sequ);
CREATE INDEX rlkey ON rl USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (rtid)::text, (lgid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX rlkey IS 'Route leg key';
CREATE INDEX rlseq ON rl USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'I03'::text, 'I17'::text], ARRAY[(crcd)::text, (rtid)::text, (sequ)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX rlseq IS 'Route leg sequence key';
CREATE UNIQUE INDEX rt_id_key ON rt USING btree (rtid);
CREATE INDEX rt_rtcd_index ON rt USING btree (rtcd);
CREATE INDEX rtcod ON rt USING btree (norm_key_val(ARRAY['C03'::text, 'A50'::text, 'I17'::text], ARRAY[(crcd)::text, (rtcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX rtcod IS 'CRCD+RTCD';
CREATE INDEX rtidk ON rt USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(rtid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX rtidk IS 'Unique id key';
CREATE INDEX rtidkindex ON rt USING btree (rtid);
CREATE INDEX rtindex ON rt USING btree (crcd, ctyp, fzid, pdid, prio, rtcd, rtid, slid, tzid);
CREATE INDEX rtkey ON rt USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'C08'::text, 'Z02'::text, 'I17'::text], ARRAY[(crcd)::text, (pdid)::text, (slid)::text, (ctyp)::text, (fzid)::text, (tzid)::text, (prio)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX rtkey IS 'CRCD+PDID+SLID+CTYP+FZID+TZID+PRIO';
CREATE INDEX rtznk ON rt USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'Z02'::text, 'I17'::text], ARRAY[(crcd)::text, (fzid)::text, (tzid)::text, (prio)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX rtznk IS 'Route zone key';
CREATE INDEX se_id_key ON se USING btree (seid);
CREATE INDEX se_secd_index ON se USING btree (secd);
CREATE INDEX search_tsv_idx ON pl USING gin (search_phrase);
CREATE INDEX seidk ON se USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(seid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX seidk IS 'Session Id Key';
CREATE INDEX seidkindex ON se USING btree (seid);
CREATE INDEX seindex ON se USING btree (secd, seid, sess);
CREATE INDEX sekey ON se USING btree (norm_key_val(ARRAY['A50'::text, 'I17'::text], ARRAY[(secd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX sekey IS 'Session Code';
CREATE INDEX seses ON se USING btree (norm_key_val(ARRAY['A50'::text, 'I17'::text], ARRAY[(sess)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX seses IS 'Session key';
CREATE UNIQUE INDEX si_id_key ON si USING btree (siid);
CREATE INDEX si_indt_index ON si USING btree (indt);
CREATE INDEX si_invn_index ON si USING btree (invn);
CREATE INDEX sidat ON si USING btree (norm_key_val(ARRAY['C03'::text, 'D11'::text, 'A20'::text, 'I17'::text], ARRAY[(crcd)::text, (date2text(indt))::text, (invn)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX sidat IS 'Invoice by creation date key';
CREATE INDEX siidk ON si USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(siid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX siidk IS 'Unique code';
CREATE INDEX siidkindex ON si USING btree (siid);
CREATE INDEX siindex ON si USING btree (crcd, indt, invn, siid);
CREATE INDEX sikey ON si USING btree (norm_key_val(ARRAY['C03'::text, 'A20'::text, 'I17'::text], ARRAY[(crcd)::text, (invn)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX sikey IS 'Invoice key';
CREATE UNIQUE INDEX sm_id_key ON sm USING btree (smid);
CREATE INDEX smidk ON sm USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(smid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX smidk IS 'Stock movement unique id';
CREATE INDEX smidkindex ON sm USING btree (smid);
CREATE INDEX smindex ON sm USING btree (crcd, csid, itid, smid, smri);
CREATE INDEX smjob ON sm USING btree (norm_key_val(ARRAY['C03'::text, 'C12'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (csid)::text, (smid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX smjob IS 'Stock movement job key';
CREATE INDEX smkey ON sm USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (itid)::text, (smri)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX smkey IS 'Stock movement key';
CREATE INDEX sq_itid_index ON sq USING btree (itid);
CREATE INDEX sq_loid_index ON sq USING btree (loid);
CREATE INDEX sqindex ON sq USING btree (crcd, itid, loid);
CREATE INDEX sqkey ON sq USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (itid)::text, (loid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX sqkey IS 'Stock item key';
CREATE INDEX sqlky ON sq USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (loid)::text, (itid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX sqlky IS 'Stock location key';
CREATE UNIQUE INDEX su_id_key ON su USING btree (suid);
CREATE INDEX su_sucd_index ON su USING btree (sucd);
CREATE INDEX sudes ON su USING btree (norm_key_val(ARRAY['C03'::text, 'A40'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX sudes IS 'CRCD+DESC';
CREATE INDEX suidk ON su USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(suid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX suidk IS 'Supplier Id Key';
CREATE INDEX suidkindex ON su USING btree (suid);
CREATE INDEX suindex ON su USING btree ("DESC", crcd, sucd, suid, type);
CREATE INDEX sukey ON su USING btree (norm_key_val(ARRAY['C03'::text, 'C06'::text, 'I17'::text], ARRAY[(crcd)::text, (sucd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX sukey IS 'CRCD+SUCD';
CREATE INDEX sutyp ON su USING btree (norm_key_val(ARRAY['C03'::text, 'S01(W:L:A)'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (type)::text, (suid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX sutyp IS 'CRCD+TYPE+SUID';
CREATE UNIQUE INDEX sy_id_key ON sy USING btree (syid);
CREATE INDEX syidk ON sy USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(syid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX syidk IS 'System Id Key';
CREATE INDEX syidkindex ON sy USING btree (syid);
CREATE INDEX syindex ON sy USING btree (code, syid);
CREATE INDEX sykey ON sy USING btree (norm_key_val(ARRAY['Z03'::text, 'I17'::text], ARRAY[(code)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX sykey IS 'System Code';
CREATE UNIQUE INDEX th_id_key ON th USING btree (thid);
CREATE INDEX th_thcd_index ON th USING btree (thcd, thdt, thtm);
CREATE INDEX thbmt ON th USING btree (norm_key_val(ARRAY['C03'::text, 'A10'::text, 'S01(N:O:P:R:X:Y:Z)'::text, 'D11'::text, 'I01'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (bmet)::text, (sent)::text, (date2text(crdt))::text, (cltr)::text, (thid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX thbmt IS 'Tracking key for booking method';
CREATE INDEX thidk ON th USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(thid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX thidk IS 'Unique id key';
CREATE INDEX thidkindex ON th USING btree (thid);
CREATE INDEX thindex ON th USING btree (bmet, cltr, crcd, crdt, husr, objt, sent, thcd, thdt, thid, thtm, tmid);
CREATE INDEX thkey ON th USING btree (norm_key_val(ARRAY['C03'::text, 'A10'::text, 'C12'::text, 'D11'::text, 'T05'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (objt)::text, (thcd)::text, (date2text(thdt))::text, (thtm)::text, (thid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX thkey IS 'Tracking key';
CREATE INDEX thque ON th USING btree (norm_key_val(ARRAY['C03'::text, 'S01(N:O:P:R:X:Y:Z)'::text, 'D11'::text, 'I01'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (sent)::text, (date2text(crdt))::text, (cltr)::text, (thid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX thque IS 'Tracking sent queue';
CREATE INDEX tmkey ON th USING btree (norm_key_val(ARRAY['C08'::text, 'C10'::text, 'I17'::text], ARRAY[(husr)::text, (tmid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX tmkey IS 'Handheld Key';
CREATE UNIQUE INDEX tr_id_key ON tr USING btree (trid);
CREATE INDEX tr_wfid_index ON tr USING btree (wfid, seqn);
CREATE INDEX tridk ON tr USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(trid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX tridk IS 'Tracking Maintenance Id Key';
CREATE INDEX tridkindex ON tr USING btree (trid);
CREATE INDEX trindex ON tr USING btree (crcd, seqn, tcod, trid, wfid);
CREATE INDEX trkey ON tr USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'A20'::text, 'I17'::text], ARRAY[(crcd)::text, (wfid)::text, (tcod)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX trkey IS 'Third party tracking code key';
CREATE INDEX trwfl ON tr USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'Z03'::text, 'I17'::text], ARRAY[(crcd)::text, (wfid)::text, (seqn)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX trwfl IS 'Tracking Workflow';
CREATE UNIQUE INDEX tx_id_key ON tx USING btree (txid);
CREATE INDEX txdes ON tx USING btree (norm_key_val(ARRAY['C03'::text, 'A30'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX txdes IS 'Tax description key';
CREATE INDEX txidk ON tx USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(txid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX txidk IS 'Sales Tax Id Key';
CREATE INDEX txidkindex ON tx USING btree (txid);
CREATE INDEX txindex ON tx USING btree ("DESC", crcd, txcd, txid);
CREATE INDEX txkey ON tx USING btree (norm_key_val(ARRAY['C03'::text, 'C03'::text, 'I17'::text], ARRAY[(crcd)::text, (txcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX txkey IS 'Courier Code+ Tax Rate Code';
CREATE UNIQUE INDEX ud_id_key ON ud USING btree (udid);
CREATE INDEX udidk ON ud USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(udid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX udidk IS 'UD Unique Key';
CREATE INDEX udidkindex ON ud USING btree (udid);
CREATE INDEX udindex ON ud USING btree (udid);
CREATE UNIQUE INDEX us_id_key ON us USING btree (usid);
CREATE INDEX us_uscd_index ON us USING btree (uscd);
CREATE INDEX uscky ON us USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'A50'::text, 'I17'::text], ARRAY[(crcd)::text, (clid)::text, (uscd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX uscky IS 'User client key';
CREATE INDEX usdes ON us USING btree (norm_key_val(ARRAY['C03'::text, 'A40'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX usdes IS 'User key';
CREATE INDEX usidk ON us USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(usid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX usidk IS 'User Id Key';
CREATE INDEX usidkindex ON us USING btree (usid);
CREATE INDEX usindex ON us USING btree ("DESC", clid, crcd, uscd, usid);
CREATE INDEX uskey ON us USING btree (norm_key_val(ARRAY['C03'::text, 'A50'::text, 'I17'::text], ARRAY[(crcd)::text, (uscd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX uskey IS 'Courier + User';
CREATE INDEX we_csid_index ON we USING btree (csid);
CREATE UNIQUE INDEX we_id_key ON we USING btree (weid);
CREATE INDEX we_tpcd_index ON we USING btree (tpcd);
CREATE INDEX we_wecd_index ON we USING btree (wecd);
CREATE INDEX weidk ON we USING btree (norm_key_val(ARRAY['C12'::text, 'I17'::text], ARRAY[(weid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX weidk IS 'Unique id key';
CREATE INDEX weidkindex ON we USING btree (weid);
CREATE INDEX weindex ON we USING btree (crcd, csid, wecd, weid);
CREATE INDEX wekey ON we USING btree (norm_key_val(ARRAY['C03'::text, 'C12'::text, 'C12'::text, 'I17'::text], ARRAY[(crcd)::text, (csid)::text, (weid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX wekey IS 'Job Pieces key';
CREATE INDEX wepky ON we USING btree (norm_key_val(ARRAY['C03'::text, 'A25'::text, 'I17'::text], ARRAY[(crcd)::text, (wecd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX wepky IS 'Piece code key';
CREATE INDEX wetky ON we USING btree (norm_key_val(ARRAY['C03'::text, 'A30'::text, 'I17'::text], ARRAY[(crcd)::text, (tpcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX wetky IS 'Piece third party ref';
CREATE UNIQUE INDEX wf_id_key ON wf USING btree (wfid);
CREATE INDEX wf_wfcd_index ON wf USING btree (wfcd);
CREATE INDEX wfdes ON wf USING btree (norm_key_val(ARRAY['C03'::text, 'A30'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX wfdes IS ' Workflow description key';
CREATE INDEX wfidk ON wf USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(wfid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX wfidk IS ' Workflow unique Id';
CREATE INDEX wfidkindex ON wf USING btree (wfid);
CREATE INDEX wfindex ON wf USING btree ("DESC", crcd, pdid, wfcd, wfid);
CREATE INDEX wfkey ON wf USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (pdid)::text, (wfcd)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX wfkey IS ' Workflow key';
CREATE INDEX wfpky ON wf USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'A30'::text, 'I17'::text], ARRAY[(crcd)::text, (pdid)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX wfpky IS 'Workflow description key';
CREATE INDEX zaindex ON za USING btree (crcd, plid, type, znid);
CREATE INDEX zakey ON za USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (znid)::text, (plid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX zakey IS 'Zone key';
CREATE INDEX zapky ON za USING btree (norm_key_val(ARRAY['C03'::text, 'C08'::text, 'C08'::text, 'S02(C:P:BO)'::text, 'I17'::text], ARRAY[(crcd)::text, (plid)::text, (znid)::text, (type)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX zapky IS 'Place key';
CREATE INDEX zatyp ON za USING btree (norm_key_val(ARRAY['C03'::text, 'S02(C:P:BO)'::text, 'C08'::text, 'C08'::text, 'I17'::text], ARRAY[(crcd)::text, (type)::text, (znid)::text, (plid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));

COMMENT ON INDEX zatyp IS 'Zone alloc type';
CREATE UNIQUE INDEX zg_id_key ON zg USING btree (zgid);

CREATE INDEX zgdes ON zg USING btree (norm_key_val(ARRAY['C03'::text, 'A30'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX zgdes IS 'Zone group description key';

CREATE INDEX zgidk ON zg USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(zgid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX zgidk IS 'Zone group unique key';

CREATE INDEX zgidkindex ON zg USING btree (zgid);

CREATE INDEX zgindex ON zg USING btree ("DESC", crcd, zgid);

CREATE UNIQUE INDEX zn_id_key ON zn USING btree (znid);

CREATE INDEX zndes ON zn USING btree (norm_key_val(ARRAY['C03'::text, 'A40'::text, 'I17'::text], ARRAY[(crcd)::text, ("DESC")::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX zndes IS 'Zone key';

CREATE INDEX znidk ON zn USING btree (norm_key_val(ARRAY['C08'::text, 'I17'::text], ARRAY[(znid)::text, ((_bookmark_ & 36028797018963967::bigint))::text]));
COMMENT ON INDEX znidk IS 'Zone unique id key';

CREATE INDEX znidkindex ON zn USING btree (znid);

CREATE INDEX znindex ON zn USING btree ("DESC", crcd, znid);

-- ---------------------------------
SET search_path = extra, pg_catalog;
CREATE TRIGGER im_jobreport_on_add_update_batch AFTER INSERT ON im_jobreport FOR EACH ROW EXECUTE PROCEDURE updatebatchtrigger();
SET search_path = public, pg_catalog;
CREATE TRIGGER on_address_update_search_phrase BEFORE INSERT OR UPDATE ON ad FOR EACH ROW EXECUTE PROCEDURE ad_update_search_pharase();
CREATE TRIGGER on_ci_patch_supplier_xxx_id BEFORE INSERT OR UPDATE ON ci FOR EACH ROW EXECUTE PROCEDURE ci_patch_supplier_xxx_id();
CREATE TRIGGER on_cl_record_update_for_search AFTER INSERT OR DELETE OR UPDATE ON cl FOR EACH ROW EXECUTE PROCEDURE on_record_update_cl();
CREATE TRIGGER on_cs_invoice_key_update BEFORE INSERT OR UPDATE ON cs FOR EACH ROW EXECUTE PROCEDURE cs_invoice_key_update();
CREATE TRIGGER on_cs_record_update_for_search AFTER INSERT OR DELETE OR UPDATE ON cs FOR EACH ROW EXECUTE PROCEDURE on_record_update_cs();
CREATE TRIGGER on_cs_update_summery AFTER INSERT OR UPDATE ON cs FOR EACH ROW WHEN ((new.clid IS NOT NULL)) EXECUTE PROCEDURE cs_update_summery();
CREATE TRIGGER on_dc_record_update_for_search AFTER INSERT OR DELETE OR UPDATE ON dc FOR EACH ROW EXECUTE PROCEDURE on_record_update_dc();
CREATE TRIGGER on_jl_update_job AFTER INSERT OR UPDATE ON jl FOR EACH ROW EXECUTE PROCEDURE jl_update_job();
CREATE TRIGGER on_nt_record_update_for_search AFTER INSERT OR DELETE OR UPDATE ON nt FOR EACH ROW EXECUTE PROCEDURE on_record_update_nt();
CREATE TRIGGER on_number_series_changed AFTER INSERT OR UPDATE ON ns FOR EACH ROW EXECUTE PROCEDURE onupdatenumberseries();
CREATE TRIGGER on_place_update_search_phrase BEFORE INSERT OR UPDATE ON pl FOR EACH ROW EXECUTE PROCEDURE pl_update_search_pharase();
CREATE TRIGGER on_tracking_history_change BEFORE INSERT OR UPDATE ON th FOR EACH ROW EXECUTE PROCEDURE trim_th_note();
CREATE TRIGGER product_awb_series_check BEFORE UPDATE ON pd FOR EACH ROW EXECUTE PROCEDURE product_awb_series_check();

-- ---------------------------------
SET search_path = extra, pg_catalog;
ALTER TABLE ONLY im_csvrow ADD CONSTRAINT fk_im_csvrow_batch FOREIGN KEY (batch) REFERENCES im_jobbatch(id);
ALTER TABLE ONLY im_jobreport ADD CONSTRAINT fk_im_jobreport_batch FOREIGN KEY (batch) REFERENCES im_jobbatch(id);

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;

