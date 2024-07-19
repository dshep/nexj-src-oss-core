--insert into test.RuleSet(id, name)
--   values (0x00000000000000000000000000000001, 'PatientIcon');

insert into test.Account(id, contactId, accountType, funds)
   values(0x00000000000000000000000000000001, 0x00000000000000000000000000000001, 'RRSP', 1000);
insert into test.Account(id, contactId, accountType, funds)
   values(0x00000000000000000000000000000002, 0x00000000000000000000000000000001, 'Canadian', 2000);
insert into test.Account(id, contactId, accountType, funds)
   values(0x00000000000000000000000000000003, 0x00000000000000000000000000000001, 'US', 3000);
insert into test.Account(id, contactId, accountType, funds)
   values(0x00000000000000000000000000000004, 0x00000000000000000000000000000002, 'RRSP', 1500);
insert into test.Account(id, contactId, accountType, funds)
   values(0x00000000000000000000000000000005, 0x00000000000000000000000000000002, 'Canadian', 2500);
insert into test.Account(id, contactId, accountType, funds)
   values(0x00000000000000000000000000000006, 0x00000000000000000000000000000002, 'US', 3500);
   
insert into test.Address(id, contactId, addr_type, country, state, city, street, code)
   values(0x00000000000000000000000000000001, 0x00000000000000000000000000000001, 'Business', 'Canada', 'ON', 'Toronto', '100 Front St E', 'M5A 1E1');
insert into test.Address(id, contactId, addr_type, country, state, city, street, code)
   values(0x00000000000000000000000000000002, 0x00000000000000000000000000000001, 'Home', 'Canada', 'ON', 'Richmond Hill', '27 Major Mackenzie Dr E', 'L4C 1G6');
insert into test.Address(id, contactId, addr_type, country, state, city, street, code)
   values(0x00000000000000000000000000000003, 0x00000000000000000000000000000002, 'Home', 'Canada', 'ON', 'Richmond Hill', '27 Major Mackenzie Dr E', 'L4C 1G6');
insert into test.Address(id, contactId, addr_type, country, state, city, street, code)
   values(0x00000000000000000000000000000004, 0x00000000000000000000000000000003, 'Rural', 'USA', 'KS', 'Wakefield', '2240 3rd Rd', '67487-9261');
insert into test.Address(id, contactId, addr_type, country, state, city, street, code)
   values(0x00000000000000000000000000000005, 0x00000000000000000000000000000003, 'Home', 'USA', 'KS', 'Sutphen', '1234 4th Rd', '67480-1234');
insert into test.Address(id, contactId, addr_type, country, state, city, street, code)
   values(0x00000000000000000000000000000006, 0x00000000000000000000000000000004, 'Business', 'Italy', null, 'Roma', 'III Forum Publicum', 'DLXV.I');
insert into test.Address(id, contactId, addr_type, country, state, city, street, code)
   values(0x00000000000000000000000000000007, 0x00000000000000000000000000000004, 'Cottage', 'Italy', null, 'Pompeii', 'Via Ampla', 'XVCC.V');

insert into test.Address(id, contactId, addr_type, country, state, city, street, code)
   values(0x00000000000000000000000000000008, 0x00000000000000000000000000000006, 'Home', 'Canada', 'ON', 'Toronto', '1900 Yonge St.', 'M4C 0J9');
insert into test.Address(id, contactId, addr_type, country, state, city, street, code)
   values(0x00000000000000000000000000000009, 0x00000000000000000000000000000008, 'Home', 'USA', 'OK', 'Tulsa', '100 Central St.', '42888');
insert into test.Address(id, contactId, addr_type, country, state, city, street, code)
   values(0x0000000000000000000000000000000A, 0x00000000000000000000000000000009, 'Home', 'USA', 'NY', 'New York', '10 Manhattan St.', '90456');
insert into test.Address(id, contactId, addr_type, country, state, city, street, code)
   values(0x0000000000000000000000000000000B, 0x0000000000000000000000000000000A, 'Business', 'Canada', 'ON', 'Vaughn', '4500 Center St.', 'L4C 9U8');
insert into test.Address(id, contactId, addr_type, country, state, city, street, code)
   values(0x0000000000000000000000000000000C, 0x0000000000000000000000000000000A, 'Home', 'Canada', 'ON', 'Vaughn', '100 Home St.', 'B9G 9U8');
insert into test.Address(id, contactId, addr_type, country, state, city, street, code)
   values(0x0000000000000000000000000000000D, 0x0000000000000000000000000000000B, 'Business', 'Canada', 'ON', 'Richmond Hill', '100 Major Mackenzy Dr.', 'L4C 0J9');
insert into test.Address(id, contactId, addr_type, country, state, city, street, code)
   values(0x0000000000000000000000000000000E, 0x0000000000000000000000000000000B, 'Home', 'Canada', 'AL', 'Edmonton', '5 Rocky Mountain Rd.', 'A5B 9J0');

insert into test.BatchJob(id, name, classCode, startTime, period, timerId, concurrency, locking)
   values(0x00000000000000000000000000000001, 'Test Workflow Queue Scheduler', 'TEST', null, null, null, 2, 0);
insert into test.BatchJob(id, name, classCode, startTime, period, timerId, concurrency, locking)
   values(0x00000000000000000000000000000002, 'Test Workflow Throttle Manager', 'WFTC', null, null, null, null, 0);


insert into test.City(name, country)
    values('Toronto', 'Canada');
insert into test.City(name, country)
    values('Edmonton', 'Canada');
insert into test.City(name, country)
    values('Richmond Hill', 'Canada');
insert into test.City(name, country)
    values('New York', 'USA');
insert into test.City(name, country)
    values('Tulsa', 'USA');

insert into test.Contact(id, primaryAddressId, readPrincipalId, title, first_name, last_name, contact_type, rec_ver, classCode, businessAddressCount)
   values(0x00000000000000000000000000000001, 0x00000000000000000000000000000001, 0x00000000000000000000000000000004, 'Mr.', 'Joe', 'Test', 'Employee', 0, 'CON', 1);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, first_name, last_name, contact_type, rec_ver, classCode, businessAddressCount)
   values(0x00000000000000000000000000000002, 0x00000000000000000000000000000003, 0x00000000000000000000000000000004, 'Mrs.', 'Zoe', 'Test', 'Employee', 0, 'CON', 0);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, first_name, last_name, contact_type, rec_ver, classCode, businessAddressCount)
   values(0x00000000000000000000000000000003, 0x00000000000000000000000000000004, 0x00000000000000000000000000000004, 'Mr.', 'John', 'Smith', 'Person', 0, 'CON', 0);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, first_name, last_name, contact_type, rec_ver, classCode, businessAddressCount)
   values(0x00000000000000000000000000000004, 0x00000000000000000000000000000006, 0x00000000000000000000000000000004, 'Mr.', 'Gaius', 'Julius', 'Person', 0, 'CON', 1);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, first_name, last_name, contact_type, rec_ver, classCode, businessAddressCount)
   values(0x00000000000000000000000000000005, null, 0x00000000000000000000000000000004, 'Mr.', 'Gnaeus', null, 'Person', 0, 'CON', 0);

insert into test.Contact(id, primaryAddressId, readPrincipalId, title, first_name, last_name, contact_type, birthdate, rec_ver, classCode, testCategory, parentId, businessAddressCount)
   values(0x00000000000000000000000000000006, null, 0x00000000000000000000000000000004, 'Mr.', 'Sarah', 'Johnson', 'Person', DATE_FORMAT('1966-04-25 12:00:00','%Y-%m-%d %H:%i:%s.%f'), 0, 'PAT', 'CLIENT', 0x00000000000000000000000000000007, 0);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, first_name, last_name, contact_type, rec_ver, classCode, testCategory, parentId, businessAddressCount)
   values(0x00000000000000000000000000000007, null, 0x00000000000000000000000000000004, 'Mr.', 'Zach', 'Tachoma', 'Person', 0, 'PAT', 'CLIENT', 0x00000000000000000000000000000008, 0);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, first_name, last_name, contact_type, rec_ver, classCode, testCategory, businessAddressCount)
   values(0x00000000000000000000000000000008, null, 0x00000000000000000000000000000004, 'Mr.', 'Andy', 'Babb', 'Person', 0, 'PAT', 'CLIENT', 0);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, first_name, last_name, contact_type, rec_ver, classCode, testCategory, businessAddressCount)
   values(0x00000000000000000000000000000009, null, 0x00000000000000000000000000000004, 'Mr.', 'John', 'Sabine', 'Person', 0, 'PAT', 'CLIENT', 0);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, first_name, last_name, contact_type, rec_ver, classCode, testCategory, businessAddressCount)
   values(0x0000000000000000000000000000000A, 0x0000000000000000000000000000000B, 0x00000000000000000000000000000004, 'Mr.', 'Johan', 'Bager', 'Person', 0, 'DOC', 'CLIENT', 1);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, first_name, last_name, contact_type, rec_ver, classCode, testCategory, businessAddressCount)
   values(0x0000000000000000000000000000000B, 0x0000000000000000000000000000000E, 0x00000000000000000000000000000004, 'Mr.', 'Michael', 'Sacco', 'Person', 0, 'DOC', 'CLIENT', 1);

insert into test.Contact(id, primaryAddressId, readPrincipalId, title, first_name, last_name, contact_type, rec_ver, classCode, businessAddressCount)
   values(0x0000000000000000000000000000000C, null, 0x00000000000000000000000000000004, 'Mr.', 'Gerhard', null, 'Person', 0, 'SPC', 0);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, first_name, last_name, contact_type, rec_ver, classCode, businessAddressCount, speciality)
   values(0x0000000000000000000000000000000D, null, 0x00000000000000000000000000000004, 'Dr.', 'Joshua', 'Zig', 'Person', 0, 'SRG', 0, 'ECG');
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, first_name, last_name, contact_type, rec_ver, classCode, testCategory, businessAddressCount)
   values(0x0000000000000000000000000000000E, null, 0x00000000000000000000000000000004, 'Dr.', 'Jeremy', 'Clarckson', 'Person', 0, 'DWP', 'CLIENT', 1);

insert into test.ContactType(type_name)
   values('Employee');
insert into test.ContactType(type_name)
   values('Person');
insert into test.ContactType(type_name)
   values('Company');

insert into test.Counter(name, value, increment, cache) values('hl7.message', 1, 1, 100);
insert into test.Counter(name, value, increment, cache) values('file.seq', 1, 1, 100);
insert into test.Counter(name, value, increment, cache) values('class.CounterTest', 1, 1, 32);

insert into test.Country(name)
    values('USA');
insert into test.Country(name)
    values('Canada');

insert into test.PetOwner(id, id2, first_name, last_name, rec_ver, relFKToVirtColl, relFKToVirtNone, relFKToVirtAttr, virtAttrToRelFKNoAttr)
   values(0x00000000000000000000000000008801, 'main', 'Joe', 'Test', 0, 0x02, 0x04, 0x03, null);
insert into test.PetOwner(id, id2, first_name, last_name, rec_ver, relFKToVirtColl, relFKToVirtNone, relFKToVirtAttr, virtAttrToRelFKNoAttr)
   values(0x00000000000000000000000000008802, 'main', 'Zoe', 'Test', 0, null, 0x03, 0x02, 0x01);

insert into test.Phone(contactId, phone_type, phone_number)
   values(0x00000000000000000000000000000001, 'Business', '(416) 2345678');
insert into test.Phone(contactId, phone_type, phone_number)
   values(0x00000000000000000000000000000001, 'Home', '(905) 7890123');

insert into test.Phone(contactId, phone_type, phone_number)
   values(0x00000000000000000000000000000006, 'Business', '(416) 777-0000');
insert into test.Phone(contactId, phone_type, phone_number)
   values(0x00000000000000000000000000000006, 'Home', '(416) 777-1111');

insert into test.Phone(contactId, phone_type, phone_number)
   values(0x00000000000000000000000000000008, 'Business', '(212) 888-0000');
insert into test.Phone(contactId, phone_type, phone_number)
   values(0x00000000000000000000000000000008, 'Home', '(212) 888-1111');

insert into test.Phone(contactId, phone_type, phone_number)
   values(0x00000000000000000000000000000009, 'Business', '(212) 999-0000');
insert into test.Phone(contactId, phone_type, phone_number)
   values(0x00000000000000000000000000000009, 'Home', '(212) 999-1111');

insert into test.Phone(contactId, phone_type, phone_number)
   values(0x0000000000000000000000000000000A, 'Business', '(212) AAA-0000');
insert into test.Phone(contactId, phone_type, phone_number)
   values(0x0000000000000000000000000000000A, 'Home', '(212) AAA-1111');

insert into test.Phone(contactId, phone_type, phone_number)
   values(0x0000000000000000000000000000000B, 'Business', '(212) BBB-0000');
insert into test.Phone(contactId, phone_type, phone_number)
   values(0x0000000000000000000000000000000B, 'Home', '(212) BBB-1111');
   
insert into test.Visit(id, startDate, endDate, reason, patientId, rec_ver)
    values(0x00000000000000000000000000000001, DATE_FORMAT('2005-01-01 10:00:00','%Y-%m-%d %H:%i:%s.%f'), DATE_FORMAT('2005-01-01 10:30:00','%Y-%m-%d %H:%i:%s.%f'), 'Broken foot', 0x00000000000000000000000000000006, 0);   
insert into test.Visit(id, startDate, endDate, reason, patientId, rec_ver)
    values(0x00000000000000000000000000000002, DATE_FORMAT('2005-01-05 13:00:00','%Y-%m-%d %H:%i:%s.%f'), DATE_FORMAT('2005-01-05 14:00:00','%Y-%m-%d %H:%i:%s.%f'), 'Broken arm', 0x00000000000000000000000000000006, 0);   
insert into test.Visit(id, startDate, endDate, reason, patientId, rec_ver)
    values(0x00000000000000000000000000000003, DATE_FORMAT('2005-01-16 11:00:00','%Y-%m-%d %H:%i:%s.%f'), null, 'Broken neck', 0x00000000000000000000000000000006, 0);   

insert into test.Visit(id, startDate, endDate, reason, patientId, rec_ver)
    values(0x00000000000000000000000000000004, DATE_FORMAT('2005-01-01 15:00:00','%Y-%m-%d %H:%i:%s.%f'), DATE_FORMAT('2005-01-01 15:30:00','%Y-%m-%d %H:%i:%s.%f'), 'Broken finger', 0x00000000000000000000000000000008, 0);   
insert into test.Visit(id, startDate, endDate, reason, patientId, rec_ver)
    values(0x00000000000000000000000000000005, DATE_FORMAT('2005-01-01 09:00:00','%Y-%m-%d %H:%i:%s.%f'), DATE_FORMAT('2005-01-01 09:44:00','%Y-%m-%d %H:%i:%s.%f'), 'Broken leg', 0x00000000000000000000000000000008, 0);   
insert into test.Visit(id, startDate, endDate, reason, patientId, rec_ver)
    values(0x00000000000000000000000000000006, DATE_FORMAT('2005-01-01 14:00:00','%Y-%m-%d %H:%i:%s.%f'), null, 'Broken shoulder', 0x00000000000000000000000000000008, 0);   
/* DO NOT ADD ANY OTHER VISITS BETWEEN Jun 20 and Jun 23, 2011, those days included.
   They WILL affect the TestCalendarTimeZone results */
insert into test.Visit(id, startDate, endDate, reason, patientId, rec_ver)
    values(x'00000000000000000000000000000007', '2011-06-21 00:01:00', '2011-06-21 23:59:00', 'All-day visit 1', x'00000000000000000000000000000008', 0);   
insert into test.Visit(id, startDate, endDate, reason, patientId, rec_ver)
    values(x'00000000000000000000000000000008', '2011-06-22 00:01:00', '2011-06-22 23:59:00', 'All-day visit 2', x'00000000000000000000000000000008', 0);   
/* SEE NOTE ABOVE before adding new visits*/

insert into test.Request(id, startDate, code, patientId, visitId, rec_ver)
    values(0x00000000000000000000000000000001, DATE_FORMAT('2005-01-15 10:00:00','%Y-%m-%d %H:%i:%s.%f'), 'CBC', 0x00000000000000000000000000000006, 0x00000000000000000000000000000002, 0);   
insert into test.Request(id, startDate, code, patientId, visitId, rec_ver)
    values(0x00000000000000000000000000000002, DATE_FORMAT('2005-01-16 13:00:00','%Y-%m-%d %H:%i:%s.%f'), 'BBC', 0x00000000000000000000000000000006, 0x00000000000000000000000000000002, 0);   
insert into test.Request(id, startDate, code, patientId, visitId, rec_ver)
    values(0x00000000000000000000000000000003, DATE_FORMAT('2005-01-17 11:00:00','%Y-%m-%d %H:%i:%s.%f'), 'YYL', 0x00000000000000000000000000000006, 0x00000000000000000000000000000002, 0);   

insert into test.Request(id, startDate, code, patientId, visitId, rec_ver)
    values(0x00000000000000000000000000000004, DATE_FORMAT('2005-01-18 14:00:00','%Y-%m-%d %H:%i:%s.%f'), 'KLK', 0x00000000000000000000000000000008, 0x00000000000000000000000000000004, 0);   
insert into test.Request(id, startDate, code, patientId, visitId, rec_ver)
    values(0x00000000000000000000000000000005, DATE_FORMAT('2005-01-19 15:00:00','%Y-%m-%d %H:%i:%s.%f'), 'PTP', 0x00000000000000000000000000000008, 0x00000000000000000000000000000004, 0);   
insert into test.Request(id, startDate, code, patientId, visitId, rec_ver)
    values(0x00000000000000000000000000000006, DATE_FORMAT('2005-01-20 16:00:00','%Y-%m-%d %H:%i:%s.%f'), 'APA', 0x00000000000000000000000000000008, 0x00000000000000000000000000000004, 0);   

insert into test.VisitParticipation(id, patientId, visitId)
    values(0x00000000000000000000000000000001, 0x00000000000000000000000000000006, 0x00000000000000000000000000000002);   
insert into test.VisitParticipation(id, patientId, visitId)
    values(0x00000000000000000000000000000002, 0x00000000000000000000000000000008, 0x00000000000000000000000000000002);   
insert into test.VisitParticipation(id, patientId, visitId)
    values(0x00000000000000000000000000000003, 0x00000000000000000000000000000009, 0x00000000000000000000000000000002);   
insert into test.VisitParticipation(id, patientId, visitId)
    values(0x00000000000000000000000000000004, 0x00000000000000000000000000000006, 0x00000000000000000000000000000003);   

insert into test.RuleDef(id, ruleSetVersionId, ordinal, name, locking, enabled, "condition", action, customized)
   values (0x00000000000000000000000000000001, 0x00000000000000000000000000000001, 1, 'Is this a company?', 0, 1,
   '(= (@ type type) "Company")', '(var''company? #t)', 0);
insert into test.RuleDef(id, ruleSetVersionId, ordinal, name, locking, enabled, "condition", action, customized)
   values (0x00000000000000000000000000000002, 0x00000000000000000000000000000001, 2, 'By default, it is not a company', 0, 1,
   null, '(var''company? #f)', 0);
insert into test.RuleDef(id, ruleSetVersionId, ordinal, name, locking, enabled, "condition", action, customized)
   values (0x00000000000000000000000000000003, 0x00000000000000000000000000000001, 3, 'gender-m-icon', 0, 1,
   '(and (not (var''company?)) (= (@ gender) "M"))', '(this''icon "icon-m")', 0);
insert into test.RuleDef(id, ruleSetVersionId, ordinal, name, locking, enabled, "condition", action, customized)
   values (0x00000000000000000000000000000004, 0x00000000000000000000000000000001, 4, 'Rule 4', 0, 1,
   '(and (not (var''company?)) (= (@ gender) "F"))', '(this''icon "icon-f")', 0);
insert into test.RuleDef(id, ruleSetVersionId, ordinal, name, locking, enabled, "condition", action, customized)
   values (0x00000000000000000000000000000005, 0x00000000000000000000000000000001, 5, 'Rule 5', 0, 1,
   '(var''company?)', '(this''icon "icon-c")', 0);
insert into test.RuleDef(id, ruleSetVersionId, ordinal, name, locking, enabled, "condition", action, customized)
   values (0x00000000000000000000000000000006, 0x00000000000000000000000000000001, 6, 'Default icon', 0, 1,
   null, '(this''icon "icon-default")', 0);

insert into test.RuleSet(id, currentVersionId, name, locking)
   values (0x00000000000000000000000000000001, 0x00000000000000000000000000000001, 'PatientIcon', 0);

insert into test.RuleSetVersion(id, ruleSetId, version, locking)
   values (0x00000000000000000000000000000001, 0x00000000000000000000000000000001, 1, 0);


insert into test.StringEnum(name, locale, value, caption)
    values('address', 'en', 'Business', 'Business');
insert into test.StringEnum(name, locale, value, caption)
    values('address', 'en', 'Home', 'Home');
insert into test.StringEnum(name, locale, value, caption)
    values('address', 'en', 'Rural', 'Rural');
insert into test.StringEnum(name, locale, value, caption)
    values('address', 'en', 'Cottage', 'Cottage');
insert into test.StringEnum(name, locale, value, caption)
    values('address', 'en', 'Residence', 'Residence');
insert into test.StringEnum(name, locale, value, caption)
    values('account', 'en', 'RRSP', 'RRSP');
insert into test.StringEnum(name, locale, value, caption)
    values('account', 'en', 'Canadian', 'Canadian');
insert into test.StringEnum(name, locale, value, caption)
    values('account', 'en', 'US', 'US');
insert into test.StringEnum(name, locale, value, caption)
    values('SSUPPORTEDLOCALE', 'en', 'en', 'en');
insert into test.StringEnum(name, locale, value, caption)
    values('SSUPPORTEDLOCALE', 'en', 'fr', 'fr');
insert into test.StringEnum(name, locale, value, caption)
    values('SSUPPORTEDLOCALE', 'fr', 'en', 'en');
insert into test.StringEnum(name, locale, value, caption)
    values('SSUPPORTEDLOCALE', 'fr', 'fr', 'fr');
insert into test.StringEnum(name, locale, value, caption)
    values('SSEVERITYENUM', 'en', 'I', 'Information');
insert into test.StringEnum(name, locale, value, caption)
    values('SSEVERITYENUM', 'en', 'W', 'Warning');    
insert into test.StringEnum(name, locale, value, caption)
    values('SSEVERITYENUM', 'en', 'E', 'Error');
insert into test.StringEnum(name, locale, value, caption)
    values('SSEVERITYENUM', 'en', 'F', 'Fatal');
insert into test.StringEnum(name, locale, value, caption)
    values('SSEVERITYENUM', 'en', 'D', 'Debug');
insert into test.StringEnum(name, locale, value, caption)
    values('SSEVERITYENUM', 'en', 'U', 'Dump');
insert into test.StringEnum(name, locale, value, caption)
    values('SMSGSTATEENUM', 'en', 'N', 'New');
insert into test.StringEnum(name, locale, value, caption)
    values('SMSGSTATEENUM', 'en', 'D', 'Dispatching');
insert into test.StringEnum(name, locale, value, caption)
    values('SMSGSTATEENUM', 'en', 'W', 'Waiting');
insert into test.StringEnum(name, locale, value, caption)
    values('SMSGSTATEENUM', 'en', 'P', 'Processing');
insert into test.StringEnum(name, locale, value, caption)
    values('SMSGSTATEENUM', 'en', 'E', 'Error');
insert into test.StringEnum(name, locale, value, caption)
    values('SMSGSTATEENUM', 'en', 'B', 'Blocked');
insert into test.StringEnum(name, locale, value, caption)
    select name, 'en_CA', value, caption from test.StringEnum where name = 'SMSGSTATEENUM' and locale = 'en';
insert into test.StringEnum(name, locale, value, caption)
    select name, 'en_GB', value, caption from test.StringEnum where name = 'SMSGSTATEENUM' and locale = 'en';
insert into test.StringEnum(name, locale, value, caption)
    select name, 'en_US', value, caption from test.StringEnum where name = 'SMSGSTATEENUM' and locale = 'en';

insert into test.UGAssoc(id, userId, groupId)
   values(0x00000000000000000000000000000001, 0x00000000000000000000000000000001, 0x00000000000000000000000000000003);
insert into test.UGAssoc(id, userId, groupId)
   values(0x00000000000000000000000000000002, 0x00000000000000000000000000000001, 0x00000000000000000000000000000004);
insert into test.UGAssoc(id, userId, groupId)
   values(0x00000000000000000000000000000003, 0x00000000000000000000000000000002, 0x00000000000000000000000000000004);

insert into test.Usr(id, contactId, parentTemplateId, managerId, tc, name, pwd, update_count)
   values(0x00000000000000000000000000000001, 0x00000000000000000000000000000001, 0x00000000000000000000000000000006, 0x00000000000000000000000000000002, 'U', 'jtest', 'secret', 0);
insert into test.Usr(id, contactId, parentTemplateId, managerId, tc, name, pwd, update_count)
   values(0x00000000000000000000000000000002, 0x00000000000000000000000000000003, 0x00000000000000000000000000000006, null, 'U', 'jsmith', 'password', 0);
insert into test.Usr(id, contactId, managerId, tc, name, pwd, update_count)
   values(0x00000000000000000000000000000003, null, null, 'G', 'QA', null, 0);
insert into test.Usr(id, contactId, managerId, tc, name, pwd, update_count)
   values(0x00000000000000000000000000000004, null, null, 'G', 'users', null, 0);
insert into test.Usr(id, contactId, managerId, tc, name, pwd, update_count)
   values(0x00000000000000000000000000000005, null, null, 'Y', 'SystemUser', null, 0);
insert into test.Usr(id, contactId, managerId, tc, name, pwd, update_count)
   values(0x00000000000000000000000000000006, null, null, 'T', 'Default', null, 0);

insert into test.UserPassword(id, userId, passwordHash, activeFlag, createdDate)
   values(0x00000000000000000000000000000001, 0x00000000000000000000000000000001, 'K7gNU3sdo+OL0wNhqoVWhr3g6s1xYv72ol/pe/Unols=', 1, null);
insert into test.UserPassword(id, userId, passwordHash, activeFlag, createdDate)
   values(0x00000000000000000000000000000002, 0x00000000000000000000000000000002, 'XohImNooBHFR0OVvjcYpJ3NgPQ1qq73WKhHvch0VQtg=', 1, null);
insert into test.UserPassword(id, userId, passwordHash, activeFlag, createdDate)
   values(0x00000000000000000000000000000003, 0x00000000000000000000000000000003, '', 1, null);
insert into test.UserPassword(id, userId, passwordHash, activeFlag, createdDate)
   values(0x00000000000000000000000000000004, 0x00000000000000000000000000000004, '', 1, null);
insert into test.UserPassword(id, userId, passwordHash, activeFlag, createdDate)
   values(0x00000000000000000000000000000005, 0x00000000000000000000000000000005, '', 1, null);
insert into test.UserPassword(id, userId, passwordHash, activeFlag, createdDate)
   values(0x00000000000000000000000000000006, 0x00000000000000000000000000000006, '', 1, null);

insert into test.UsrPriv(id, userId, name)
   values(0x00000000000000000000000000000001, 0x00000000000000000000000000000001, 'contactEntry');
insert into test.UsrPriv(id, userId, name)
   values(0x00000000000000000000000000000002, 0x00000000000000000000000000000001, 'contactManagement');
insert into test.UsrPriv(id, userId, name)
   values(0x00000000000000000000000000000003, 0x00000000000000000000000000000001, 'dummyRead');
insert into test.UsrPriv(id, userId, name)
   values(0x00000000000000000000000000000004, 0x00000000000000000000000000000001, 'dummyUpdate');
insert into test.UsrPriv(id, userId, name)
   values(0x00000000000000000000000000000005, 0x00000000000000000000000000000001, 'readAddress');
insert into test.UsrPriv(id, userId, name)
   values(0x00000000000000000000000000000006, 0x00000000000000000000000000000001, 'addressContactRead');
insert into test.UsrPriv(id, userId, name)
   values(0x00000000000000000000000000000007, 0x00000000000000000000000000000001, 'BatchJobRun');
insert into test.UsrPriv(id, userId, name)
   values(0x00000000000000000000000000000008, 0x00000000000000000000000000000001, 'BatchJobManage');
insert into test.UsrPriv(id, userId, name)
   values(0x00000000000000000000000000000009, 0x00000000000000000000000000000001, 'WorkflowManage');
insert into test.UsrPriv(id, userId, name)
   values(0x0000000000000000000000000000000A, 0x00000000000000000000000000000001, 'updatePatient');
insert into test.UsrPriv(id, userId, name)
   values(0x0000000000000000000000000000000B, 0x00000000000000000000000000000001, 'deletePatient');
insert into test.UsrPriv(id, userId, name)
   values(0x0000000000000000000000000000000C, 0x00000000000000000000000000000001, 'childrenUpdate');

insert into test.Version(namespace, version, step, upgradable, test, loaded, locking) values ('test', '-1', -1, 1, 1, 1, 1);

insert into test.Queue(id, name, classCode, caption, locking, customized, system)
   values(0x00000000000000000000000000000001, 'lifo', 'WFLIFOQueue', 'LIFO', 0, 0, 1);
insert into test.WFQueue(id)
   values(0x00000000000000000000000000000001);
insert into test.Queue(id, name, classCode, caption, locking, customized, system)
   values(0x00000000000000000000000000000002, 'fifo', 'WFFIFOQueue', 'FIFO', 0, 0, 1);
insert into test.WFQueue(id)
   values(0x00000000000000000000000000000002);
insert into test.Queue(id, name, classCode, caption, locking, customized, system, concurrency, priority)
   values(0x00000000000000000000000000000003, 'Semaphore', 'WFWindowedFIFOQueue', 'Semaphore', 0, 0, 1, 2, 10);
insert into test.WFQueue(id, schedulerId)
   values(0x00000000000000000000000000000003, 0x00000000000000000000000000000001);
insert into test.Queue(id, name, classCode, caption, locking, customized, system, concurrency, priority)
   values(0x00000000000000000000000000000004, 'SemaphoreLowConcurrency', 'WFWindowedFIFOQueue', 'SemaphoreLowConcurrency', 0, 0, 1, 1, 20);
insert into test.WFQueue(id, schedulerId)
   values(0x00000000000000000000000000000004, 0x00000000000000000000000000000001);
insert into test.Queue(id, name, classCode, caption, locking, customized, system, concurrency, priority, throttleCounterId)
   values(0x00000000000000000000000000000005, 'SemaphoreThrottled', 'WFWindowedFIFOQueue', 'SemaphoreThrottled', 0, 0, 1, NULL, 5, 0x00000000000000000000000000000001);
insert into test.WFQueue(id, schedulerId)
   values(0x00000000000000000000000000000005, 0x00000000000000000000000000000001);
insert into test.Queue (id, classCode, name, caption, priority, customized, system, locking) 
   values (0x00000000000000000000000000000006, 'ObjQueue', 'ObjectDispatcherQueue', 'idsc.rpc.queueing.dispatcherQueue', 0, 0, 1, 0);
insert into test.Queue (id, classCode, name, caption, priority, customized, system, locking) 
   values (0x00000000000000000000000000000007, 'TestQueue', 'CommandRequestQueue', '', 0, 0, 1, 0);
insert into test.Queue (id, classCode, name, caption, priority, customized, system, locking) 
   values (0x00000000000000000000000000000008, 'ObjQueue', 'SysMessageQueue', '', 0, 0, 1, 0);
insert into test.Queue (id, classCode, name, caption, priority, customized, system, locking) 
   values (0x00000000000000000000000000000009, 'ObjQueue', 'SignalErrorQueue', '', 0, 0, 1, 0);
insert into test.Queue (id, classCode, name, caption, priority, customized, system, locking) 
   values (0x0000000000000000000000000000000A, 'ObjQueue', 'ObjectSystemQueue', '', 0, 0, 1, 0);

insert into test.ObjectQueue (id, timeout, sendEnabled, receiveEnabled, errorCount) 
   values (0x00000000000000000000000000000006, 6000, 1, 1, 1);
insert into test.ObjectQueue (id, timeout, sendEnabled, receiveEnabled, errorCount, errorQueueId) 
   values (0x00000000000000000000000000000007, 6000, 1, 1, 2, 0x00000000000000000000000000000009);
insert into test.ObjectQueue (id, timeout, sendEnabled, receiveEnabled, errorCount, errorQueueId) 
   values (0x00000000000000000000000000000008, 6000, 1, 1, 2, 0x00000000000000000000000000000009);
insert into test.ObjectQueue (id, timeout, sendEnabled, receiveEnabled, errorCount) 
   values (0x00000000000000000000000000000009, 6000, 1, 1, 1);
insert into test.ObjectQueue (id, timeout, sendEnabled, receiveEnabled, errorCount) 
   values (0x0000000000000000000000000000000A, 6000, 1, 1, 1);
   
insert into test.ThrottleCounter(id, cnt, rate, batchJobId, locking)
   values(0x00000000000000000000000000000001, 2, 2, 0x00000000000000000000000000000002, 0);

insert into test.ObjectQueueDispatcher (id) 
   values (0x00000000000000000000000000000001);

-- Same as BatchJob id
insert into test.WFSchedulerLock(id)
   values(0x00000000000000000000000000000001);
