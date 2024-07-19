--insert into test.RuleSet(id, name)
--   values (decode('00000000000000000000000000000001','hex'), 'PatientIcon');

insert into test.Account(id, contactId, accountType, funds)
   values(decode('00000000000000000000000000000001','hex'), decode('00000000000000000000000000000001','hex'), 'RRSP', 1000);
insert into test.Account(id, contactId, accountType, funds)
   values(decode('00000000000000000000000000000002','hex'), decode('00000000000000000000000000000001','hex'), 'Canadian', 2000);
insert into test.Account(id, contactId, accountType, funds)
   values(decode('00000000000000000000000000000003','hex'), decode('00000000000000000000000000000001','hex'), 'US', 3000);
insert into test.Account(id, contactId, accountType, funds)
   values(decode('00000000000000000000000000000004','hex'), decode('00000000000000000000000000000002','hex'), 'RRSP', 1500);
insert into test.Account(id, contactId, accountType, funds)
   values(decode('00000000000000000000000000000005','hex'), decode('00000000000000000000000000000002','hex'), 'Canadian', 2500);
insert into test.Account(id, contactId, accountType, funds)
   values(decode('00000000000000000000000000000006','hex'), decode('00000000000000000000000000000002','hex'), 'US', 3500);
   
insert into test.Address(id, contactId, addr_type, country, state, state$, city, street, street$, code, code$)
   values(decode('00000000000000000000000000000001','hex'), decode('00000000000000000000000000000001','hex'), 'Business', 'Canada', 'ON', upper('ON'), 'Toronto', '100 Front St E', upper('100 Front St E'), 'M5A 1E1', upper('M5A 1E1'));
insert into test.Address(id, contactId, addr_type, country, state, state$, city, street, street$, code, code$)
   values(decode('00000000000000000000000000000002','hex'), decode('00000000000000000000000000000001','hex'), 'Home', 'Canada', 'ON', upper('ON'), 'Richmond Hill', '27 Major Mackenzie Dr E', upper('27 Major Mackenzie Dr E'), 'L4C 1G6', upper('L4C 1G6'));
insert into test.Address(id, contactId, addr_type, country, state, state$, city, street, street$, code, code$)
   values(decode('00000000000000000000000000000003','hex'), decode('00000000000000000000000000000002','hex'), 'Home', 'Canada', 'ON', upper('ON'), 'Richmond Hill', '27 Major Mackenzie Dr E', upper('27 Major Mackenzie Dr E'), 'L4C 1G6', upper('L4C 1G6'));
insert into test.Address(id, contactId, addr_type, country, state, state$, city, street, street$, code, code$)
   values(decode('00000000000000000000000000000004','hex'), decode('00000000000000000000000000000003','hex'), 'Rural', 'USA', 'KS', upper('KS'), 'Wakefield', '2240 3rd Rd', upper('2240 3rd Rd'), '67487-9261', upper('67487-9261'));
insert into test.Address(id, contactId, addr_type, country, state, state$, city, street, street$, code, code$)
   values(decode('00000000000000000000000000000005','hex'), decode('00000000000000000000000000000003','hex'), 'Home', 'USA', 'KS', upper('KS'), 'Sutphen', '1234 4th Rd', upper('1234 4th Rd'), '67480-1234', upper('67480-1234'));
insert into test.Address(id, contactId, addr_type, country, state, state$, city, street, street$, code, code$)
   values(decode('00000000000000000000000000000006','hex'), decode('00000000000000000000000000000004','hex'), 'Business', 'Italy', null, lower(null), 'Roma', 'III Forum Publicum', upper('III Forum Publicum'), 'DLXV.I', upper('DLXV.I'));
insert into test.Address(id, contactId, addr_type, country, state, state$, city, street, street$, code, code$)
   values(decode('00000000000000000000000000000007','hex'), decode('00000000000000000000000000000004','hex'), 'Cottage', 'Italy', null, lower(null), 'Pompeii', 'Via Ampla', upper('Via Ampla'), 'XVCC.V', upper('XVCC.V'));
insert into test.Address(id, contactId, addr_type, country, state, state$, city, street, street$, code, code$)
   values(decode('00000000000000000000000000000008','hex'), decode('00000000000000000000000000000006','hex'), 'Home', 'Canada', 'ON', upper('ON'), 'Toronto', '1900 Yonge St.', upper('1900 Yonge St.'), 'M4C 0J9', upper('M4C 0J9'));
insert into test.Address(id, contactId, addr_type, country, state, state$, city, street, street$, code, code$)
   values(decode('00000000000000000000000000000009','hex'), decode('00000000000000000000000000000008','hex'), 'Home', 'USA', 'OK', upper('OK'), 'Tulsa', '100 Central St.', upper('100 Central St.'), '42888', upper('42888'));
insert into test.Address(id, contactId, addr_type, country, state, state$, city, street, street$, code, code$)
   values(decode('0000000000000000000000000000000A','hex'), decode('00000000000000000000000000000009','hex'), 'Home', 'USA', 'NY', upper('NY'), 'New York', '10 Manhattan St.', upper('10 Manhattan St.'), '90456', upper('90456'));
insert into test.Address(id, contactId, addr_type, country, state, state$, city, street, street$, code, code$)
   values(decode('0000000000000000000000000000000B','hex'), decode('0000000000000000000000000000000A','hex'), 'Business', 'Canada', 'ON', upper('ON'), 'Vaughn', '4500 Center St.', upper('4500 Center St.'), 'L4C 9U8', upper('L4C 9U8'));
insert into test.Address(id, contactId, addr_type, country, state, state$, city, street, street$, code, code$)
   values(decode('0000000000000000000000000000000C','hex'), decode('0000000000000000000000000000000A','hex'), 'Home', 'Canada', 'ON', upper('ON'), 'Vaughn', '100 Home St.', upper('100 Home St.'), 'B9G 9U8', upper('B9G 9U8'));
insert into test.Address(id, contactId, addr_type, country, state, state$, city, street, street$, code, code$)
   values(decode('0000000000000000000000000000000D','hex'), decode('0000000000000000000000000000000B','hex'), 'Business', 'Canada', 'ON', upper('ON'), 'Richmond Hill', '100 Major Mackenzy Dr.', upper('100 Major Mackenzy Dr.'), 'L4C 0J9', upper('L4C 0J9'));
insert into test.Address(id, contactId, addr_type, country, state, state$, city, street, street$, code, code$)
   values(decode('0000000000000000000000000000000E','hex'), decode('0000000000000000000000000000000B','hex'), 'Home', 'Canada', 'AL', upper('AL'), 'Edmonton', '5 Rocky Mountain Rd.', 'A5B 9J0', upper('A5B 9J0'), upper('A5B 9J0'));

insert into test.BatchJob(id, name, name$, classCode, startTime, period, timerId, concurrency, locking)
   values(decode('00000000000000000000000000000001','hex'), 'Test Workflow Queue Scheduler', upper('Test Workflow Queue Scheduler'), 'TEST', null, null, null, 2, 0);
insert into test.BatchJob(id, name, name$, classCode, startTime, period, timerId, concurrency, locking)
   values(decode('00000000000000000000000000000002','hex'), 'Test Workflow Throttle Manager', upper('Test Workflow Throttle Manager'), 'WFTC', null, null, null, null, 0);

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

insert into test.Contact(id, primaryAddressId, readPrincipalId, title, title$, first_name, first_name$, last_name, last_name$, contact_type, rec_ver, classCode, businessAddressCount)
   values(decode('00000000000000000000000000000001','hex'), decode('00000000000000000000000000000001','hex'), decode('00000000000000000000000000000004','hex'), 'Mr.', upper('Mr.'), 'Joe', upper('Joe'), 'Test', upper('Test'), 'Employee', 0, 'CON', 1);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, title$, first_name, first_name$, last_name, last_name$, contact_type, rec_ver, classCode, businessAddressCount)
   values(decode('00000000000000000000000000000002','hex'), decode('00000000000000000000000000000003','hex'), decode('00000000000000000000000000000004','hex'), 'Mrs.', upper('Mrs.'), 'Zoe', upper('Zoe'), 'Test', upper('Test'), 'Employee', 0, 'CON', 0);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, title$, first_name, first_name$, last_name, last_name$, contact_type, rec_ver, classCode, businessAddressCount)
   values(decode('00000000000000000000000000000003','hex'), decode('00000000000000000000000000000004','hex'), decode('00000000000000000000000000000004','hex'), 'Mr.', upper('Mr.'), 'John', upper('John'), 'Smith', upper('Smith'), 'Person', 0, 'CON', 0);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, title$, first_name, first_name$, last_name, last_name$, contact_type, rec_ver, classCode, businessAddressCount)
   values(decode('00000000000000000000000000000004','hex'), decode('00000000000000000000000000000006','hex'), decode('00000000000000000000000000000004','hex'), 'Mr.', upper('Mr.'), 'Gaius', upper('Gaius'), 'Julius', upper('Julius'), 'Person', 0, 'CON', 1);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, title$, first_name, first_name$, last_name, last_name$, contact_type, rec_ver, classCode, businessAddressCount)
   values(decode('00000000000000000000000000000005','hex'), null, decode('00000000000000000000000000000004','hex'), 'Mr.', upper('Mr.'), 'Gnaeus', upper('Gnaeus'), null, lower(null), 'Person', 0, 'CON', 0);

insert into test.Contact(id, primaryAddressId, readPrincipalId, title, title$, first_name, first_name$, last_name, last_name$, contact_type, birthdate, rec_ver, classCode, testCategory, parentId, businessAddressCount)
   values(decode('00000000000000000000000000000006','hex'), null, decode('00000000000000000000000000000004','hex'), 'Mr.', upper('Mr.'), 'Sarah', upper('Sarah'), 'Johnson', upper('Johnson'), 'Person', timestamptz'1966-04-25 12:00:00+00', 0, 'PAT', 'CLIENT', decode('00000000000000000000000000000007','hex'), 0);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, title$, first_name, first_name$, last_name, last_name$, contact_type, rec_ver, classCode, testCategory, parentId, businessAddressCount)
   values(decode('00000000000000000000000000000007','hex'), null, decode('00000000000000000000000000000004','hex'), 'Mr.', upper('Mr.'), 'Zach', upper('Zach'), 'Tachoma', upper('Tachoma'), 'Person', 0, 'PAT', 'CLIENT', decode('00000000000000000000000000000008','hex'), 0);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, title$, first_name, first_name$, last_name, last_name$, contact_type, rec_ver, classCode, testCategory, businessAddressCount)
   values(decode('00000000000000000000000000000008','hex'), null, decode('00000000000000000000000000000004','hex'), 'Mr.', upper('Mr.'), 'Andy', upper('Andy'), 'Babb', upper('Babb'), 'Person', 0, 'PAT', 'CLIENT', 0);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, title$, first_name, first_name$, last_name, last_name$, contact_type, rec_ver, classCode, testCategory, businessAddressCount)
   values(decode('00000000000000000000000000000009','hex'), null, decode('00000000000000000000000000000004','hex'), 'Mr.', upper('Mr.'), 'John', upper('John'), 'Sabine', upper('Sabine'), 'Person', 0, 'PAT', 'CLIENT', 0);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, title$, first_name, first_name$, last_name, last_name$, contact_type, rec_ver, classCode, testCategory, businessAddressCount)
   values(decode('0000000000000000000000000000000A','hex'), decode('0000000000000000000000000000000B','hex'), decode('00000000000000000000000000000004','hex'), 'Mr.', upper('Mr.'), 'Johan', upper('Johan'), 'Bager', upper('Bager'), 'Person', 0, 'DOC', 'CLIENT', 1);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, title$, first_name, first_name$, last_name, last_name$, contact_type, rec_ver, classCode, testCategory, businessAddressCount)
   values(decode('0000000000000000000000000000000B','hex'), decode('0000000000000000000000000000000E','hex'), decode('00000000000000000000000000000004','hex'), 'Mr.', upper('Mr.'), 'Michael', upper('Michael'), 'Sacco', upper('Sacco'), 'Person', 0, 'DOC', 'CLIENT', 1);

insert into test.Contact(id, primaryAddressId, readPrincipalId, title, title$, first_name, first_name$, last_name, last_name$, contact_type, rec_ver, classCode, businessAddressCount)
   values(decode('0000000000000000000000000000000C','hex'), null, decode('00000000000000000000000000000004','hex'), 'Mr.', upper('Mr.'), 'Gerhard', upper('Gerhard'), null, lower(null), 'Person', 0, 'SPC', 0);
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, title$, first_name, first_name$, last_name, last_name$, contact_type, rec_ver, classCode, businessAddressCount, speciality)
   values(decode('0000000000000000000000000000000D','hex'), null, decode('00000000000000000000000000000004','hex'), 'Dr.', upper('Dr.'), 'Joshua', upper('Joshua'), 'Zig', upper('Zig'), 'Person', 0, 'SRG', 0, 'ECG');
insert into test.Contact(id, primaryAddressId, readPrincipalId, title, title$, first_name, first_name$, last_name, last_name$, contact_type, rec_ver, classCode, testCategory, businessAddressCount)
   values(decode('0000000000000000000000000000000E','hex'), null, decode('00000000000000000000000000000004','hex'), 'Dr.', upper('Dr.'), 'Jeremy', upper('Jeremy'), 'Clarckson', upper('Clarckson'), 'Person', 0, 'DWP', 'CLIENT', 1);

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

insert into test.PetOwner(id, id2, first_name, first_name$, last_name, last_name$, rec_ver, relFKToVirtColl, relFKToVirtNone, relFKToVirtAttr, virtAttrToRelFKNoAttr)
   values(decode('00000000000000000000000000008801','hex'), 'main', 'Joe', upper('Joe'), 'Test', upper('Test'), 0, decode('02','hex'), decode('04','hex'), decode('03','hex'), null);
insert into test.PetOwner(id, id2, first_name, first_name$, last_name, last_name$, rec_ver, relFKToVirtColl, relFKToVirtNone, relFKToVirtAttr, virtAttrToRelFKNoAttr)
   values(decode('00000000000000000000000000008802','hex'), 'main', 'Zoe', upper('Zoe'), 'Test', upper('Test'), 0, null, decode('03','hex'), decode('02','hex'), decode('01','hex'));

insert into test.Phone(contactId, phone_type, phone_type$, phone_number, phone_number$)
   values(decode('00000000000000000000000000000001','hex'), 'Business', upper('Business'), '(416) 2345678', upper('(416) 2345678'));
insert into test.Phone(contactId, phone_type, phone_type$, phone_number, phone_number$)
   values(decode('00000000000000000000000000000001','hex'), 'Home', upper('Home'), '(905) 7890123', upper('(905) 7890123'));

insert into test.Phone(contactId, phone_type, phone_type$, phone_number, phone_number$)
   values(decode('00000000000000000000000000000006','hex'), 'Business', upper('Business'), '(416) 777-0000', upper('(416) 777-0000'));
insert into test.Phone(contactId, phone_type, phone_type$, phone_number, phone_number$)
   values(decode('00000000000000000000000000000006','hex'), 'Home', upper('Home'), '(416) 777-1111', upper('(416) 777-1111'));

insert into test.Phone(contactId, phone_type, phone_type$, phone_number, phone_number$)
   values(decode('00000000000000000000000000000008','hex'), 'Business', upper('Business'), '(212) 888-0000', upper('(212) 888-0000'));
insert into test.Phone(contactId, phone_type, phone_type$, phone_number, phone_number$)
   values(decode('00000000000000000000000000000008','hex'), 'Home', upper('Home'), '(212) 888-1111', upper('(212) 888-1111'));

insert into test.Phone(contactId, phone_type, phone_type$, phone_number, phone_number$)
   values(decode('00000000000000000000000000000009','hex'), 'Business', upper('Business'), '(212) 999-0000', upper('(212) 999-0000'));
insert into test.Phone(contactId, phone_type, phone_type$, phone_number, phone_number$)
   values(decode('00000000000000000000000000000009','hex'), 'Home', upper('Home'), '(212) 999-1111', upper('(212) 999-1111'));

insert into test.Phone(contactId, phone_type, phone_type$, phone_number, phone_number$)
   values(decode('0000000000000000000000000000000A','hex'), 'Business', upper('Business'), '(212) AAA-0000', upper('(212) AAA-0000'));
insert into test.Phone(contactId, phone_type, phone_type$, phone_number, phone_number$)
   values(decode('0000000000000000000000000000000A','hex'), 'Home', upper('Home'), '(212) AAA-1111', upper('(212) AAA-1111'));

insert into test.Phone(contactId, phone_type, phone_type$, phone_number, phone_number$)
   values(decode('0000000000000000000000000000000B','hex'), 'Business', upper('Business'), '(212) BBB-0000', upper('(212) BBB-0000'));
insert into test.Phone(contactId, phone_type, phone_type$, phone_number, phone_number$)
   values(decode('0000000000000000000000000000000B','hex'), 'Home', upper('Home'), '(212) BBB-1111', upper('(212) BBB-1111'));
   
insert into test.Visit(id, startDate, endDate, reason, reason$, patientId, rec_ver)
    values(decode('00000000000000000000000000000001','hex'), timestamptz'2005-01-01 10:00:00+00', timestamptz'2005-01-01 10:30:00+00', 'Broken foot', upper('Broken foot'), decode('00000000000000000000000000000006','hex'), 0);   
insert into test.Visit(id, startDate, endDate, reason, reason$, patientId, rec_ver)
    values(decode('00000000000000000000000000000002','hex'), timestamptz'2005-01-05 13:00:00+00', timestamptz'2005-01-05 14:00:00+00', 'Broken arm', upper('Broken arm'), decode('00000000000000000000000000000006','hex'), 0);   
insert into test.Visit(id, startDate, endDate, reason, reason$, patientId, rec_ver)
    values(decode('00000000000000000000000000000003','hex'), timestamptz'2005-01-16 11:00:00+00', null, 'Broken neck', upper('Broken neck'), decode('00000000000000000000000000000006','hex'), 0);   

insert into test.Visit(id, startDate, endDate, reason, reason$, patientId, rec_ver)
    values(decode('00000000000000000000000000000004','hex'), timestamptz'2005-01-01 15:00:00+00', timestamptz'2005-01-01 15:30:00+00', 'Broken finger', upper('Broken finger'), decode('00000000000000000000000000000008','hex'), 0);   
insert into test.Visit(id, startDate, endDate, reason, reason$, patientId, rec_ver)
    values(decode('00000000000000000000000000000005','hex'), timestamptz'2005-01-01 09:00:00+00', timestamptz'2005-01-01 09:44:00+00', 'Broken leg', upper('Broken leg'), decode('00000000000000000000000000000008','hex'), 0);   
insert into test.Visit(id, startDate, endDate, reason, reason$, patientId, rec_ver)
    values(decode('00000000000000000000000000000006','hex'), timestamptz'2005-01-01 14:00:00+00', null, 'Broken shoulder', upper('Broken shoulder'), decode('00000000000000000000000000000008','hex'), 0);   
/* DO NOT ADD ANY OTHER VISITS BETWEEN Jun 20 and Jun 23, 2011, those days included.
   They WILL affect the TestCalendarTimeZone results */
insert into test.Visit(id, startDate, endDate, reason, reason$, patientId, rec_ver)
    values(decode('00000000000000000000000000000007','hex'), timestamptz'2011-06-21 00:01:00', timestamptz'2011-06-21 23:59:00', 'All-day visit 1', upper('All-day visit 1'), decode('00000000000000000000000000000008','hex'), 0);   
insert into test.Visit(id, startDate, endDate, reason, reason$, patientId, rec_ver)
    values(decode('00000000000000000000000000000008','hex'), timestamptz'2011-06-22 00:01:00', timestamptz'2011-06-22 23:59:00', 'All-day visit 2', upper('All-day visit 2'), decode('00000000000000000000000000000008','hex'), 0);   
/* SEE NOTE ABOVE before adding new visits*/

insert into test.Request(id, startDate, code, code$, patientId, visitId, rec_ver)
    values(decode('00000000000000000000000000000001','hex'), timestamptz'2005-01-15 10:00:00+00', 'CBC', upper('CBC'), decode('00000000000000000000000000000006','hex'), decode('00000000000000000000000000000002','hex'), 0);   
insert into test.Request(id, startDate, code, code$, patientId, visitId, rec_ver)
    values(decode('00000000000000000000000000000002','hex'), timestamptz'2005-01-16 13:00:00+00', 'BBC', upper('BBC'), decode('00000000000000000000000000000006','hex'), decode('00000000000000000000000000000002','hex'), 0);   
insert into test.Request(id, startDate, code, code$, patientId, visitId, rec_ver)
    values(decode('00000000000000000000000000000003','hex'), timestamptz'2005-01-17 11:00:00+00', 'YYL', upper('YYL'), decode('00000000000000000000000000000006','hex'), decode('00000000000000000000000000000002','hex'), 0);   

insert into test.Request(id, startDate, code, code$, patientId, visitId, rec_ver)
    values(decode('00000000000000000000000000000004','hex'), timestamptz'2005-01-18 14:00:00+00', 'KLK', upper('KLK'), decode('00000000000000000000000000000008','hex'), decode('00000000000000000000000000000004','hex'), 0);   
insert into test.Request(id, startDate, code, code$, patientId, visitId, rec_ver)
    values(decode('00000000000000000000000000000005','hex'), timestamptz'2005-01-19 15:00:00+00', 'PTP', upper('PTP'), decode('00000000000000000000000000000008','hex'), decode('00000000000000000000000000000004','hex'), 0);   
insert into test.Request(id, startDate, code, code$, patientId, visitId, rec_ver)
    values(decode('00000000000000000000000000000006','hex'), timestamptz'2005-01-20 16:00:00+00', 'APA', upper('APA'), decode('00000000000000000000000000000008','hex'), decode('00000000000000000000000000000004','hex'), 0);   

insert into test.VisitParticipation(id, patientId, visitId)
    values(decode('00000000000000000000000000000001','hex'), decode('00000000000000000000000000000006','hex'), decode('00000000000000000000000000000002','hex'));   
insert into test.VisitParticipation(id, patientId, visitId)
    values(decode('00000000000000000000000000000002','hex'), decode('00000000000000000000000000000008','hex'), decode('00000000000000000000000000000002','hex'));   
insert into test.VisitParticipation(id, patientId, visitId)
    values(decode('00000000000000000000000000000003','hex'), decode('00000000000000000000000000000009','hex'), decode('00000000000000000000000000000002','hex'));   
insert into test.VisitParticipation(id, patientId, visitId)
    values(decode('00000000000000000000000000000004','hex'), decode('00000000000000000000000000000006','hex'), decode('00000000000000000000000000000003','hex'));   

insert into test.RuleDef(id, ruleSetVersionId, ordinal, name, locking, enabled, condition, action, customized)
   values (decode('00000000000000000000000000000001','hex'), decode('00000000000000000000000000000001','hex'), 1, 'Is this a company?', 0, TRUE,
   '(= (@ type type) "Company")', '(var''company? #t)', TRUE);
insert into test.RuleDef(id, ruleSetVersionId, ordinal, name, locking, enabled, condition, action, customized)
   values (decode('00000000000000000000000000000002','hex'), decode('00000000000000000000000000000001','hex'), 2, 'By default, it is not a company', 0, TRUE,
   null, '(var''company? #f)', FALSE);
insert into test.RuleDef(id, ruleSetVersionId, ordinal, name, locking, enabled, condition, action, customized)
   values (decode('00000000000000000000000000000003','hex'), decode('00000000000000000000000000000001','hex'), 3, 'gender-m-icon', 0, TRUE,
   '(and (not (var''company?)) (= (@ gender) "M"))', '(this''icon "icon-m")', FALSE);
insert into test.RuleDef(id, ruleSetVersionId, ordinal, name, locking, enabled, condition, action, customized)
   values (decode('00000000000000000000000000000004','hex'), decode('00000000000000000000000000000001','hex'), 4, 'Rule 4', 0, TRUE,
   '(and (not (var''company?)) (= (@ gender) "F"))', '(this''icon "icon-f")', FALSE);
insert into test.RuleDef(id, ruleSetVersionId, ordinal, name, locking, enabled, condition, action, customized)
   values (decode('00000000000000000000000000000005','hex'), decode('00000000000000000000000000000001','hex'), 5, 'Rule 5', 0, TRUE,
   '(var''company?)', '(this''icon "icon-c")', FALSE);
insert into test.RuleDef(id, ruleSetVersionId, ordinal, name, locking, enabled, condition, action, customized)
   values (decode('00000000000000000000000000000006','hex'), decode('00000000000000000000000000000001','hex'), 6, 'Default icon', 0, TRUE,
   null, '(this''icon "icon-default")', FALSE);

insert into test.RuleSet(id, currentVersionId, name, locking)
   values (decode('00000000000000000000000000000001','hex'), decode('00000000000000000000000000000001','hex'), 'PatientIcon', 0);

insert into test.RuleSetVersion(id, ruleSetId, version, locking)
   values (decode('00000000000000000000000000000001','hex'), decode('00000000000000000000000000000001','hex'), 1, 0);


insert into test.StringEnum(name, locale, value, caption, caption$)
    values('address', 'en', 'Business', 'Business', upper('Business'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('address', 'en', 'Home', 'Home', upper('Home'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('address', 'en', 'Rural', 'Rural', upper('Rural'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('address', 'en', 'Cottage', 'Cottage', upper('Cottage'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('address', 'en', 'Residence', 'Residence', upper('Residence'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('account', 'en', 'RRSP', 'RRSP', upper('RRSP'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('account', 'en', 'Canadian', 'Canadian', upper('Canadian'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('account', 'en', 'US', 'US', upper('US'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SSUPPORTEDLOCALE', 'en', 'en', 'en', upper('en'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SSUPPORTEDLOCALE', 'en', 'fr', 'fr', upper('fr'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SSUPPORTEDLOCALE', 'fr', 'en', 'en', upper('en'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SSUPPORTEDLOCALE', 'fr', 'fr', 'fr', upper('fr'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SSEVERITYENUM', 'en', 'I', 'Information', upper('Information'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SSEVERITYENUM', 'en', 'W', 'Warning', upper('Warning'));    
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SSEVERITYENUM', 'en', 'E', 'Error', upper('Error'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SSEVERITYENUM', 'en', 'F', 'Fatal', upper('Fatal'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SSEVERITYENUM', 'en', 'D', 'Debug', upper('Debug'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SSEVERITYENUM', 'en', 'U', 'Dump', upper('Dump'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SMSGSTATEENUM', 'en', 'N', 'New', upper('New'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SMSGSTATEENUM', 'en', 'D', 'Dispatching', upper('Dispatching'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SMSGSTATEENUM', 'en', 'W', 'Waiting', upper('Waiting'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SMSGSTATEENUM', 'en', 'P', 'Processing', upper('Processing'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SMSGSTATEENUM', 'en', 'E', 'Error', upper('Error'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    values('SMSGSTATEENUM', 'en', 'B', 'Blocked', upper('Blocked'));
insert into test.StringEnum(name, locale, value, caption, caption$)
    select name, 'en_CA', value, caption, caption$ from test.StringEnum where name = 'SMSGSTATEENUM' and locale = 'en';
insert into test.StringEnum(name, locale, value, caption, caption$)
    select name, 'en_GB', value, caption, caption$ from test.StringEnum where name = 'SMSGSTATEENUM' and locale = 'en';
insert into test.StringEnum(name, locale, value, caption, caption$)
    select name, 'en_US', value, caption, caption$ from test.StringEnum where name = 'SMSGSTATEENUM' and locale = 'en';

insert into test.UGAssoc(id, userId, groupId)
   values(decode('00000000000000000000000000000001','hex'), decode('00000000000000000000000000000001','hex'), decode('00000000000000000000000000000003','hex'));
insert into test.UGAssoc(id, userId, groupId)
   values(decode('00000000000000000000000000000002','hex'), decode('00000000000000000000000000000001','hex'), decode('00000000000000000000000000000004','hex'));
insert into test.UGAssoc(id, userId, groupId)
   values(decode('00000000000000000000000000000003','hex'), decode('00000000000000000000000000000002','hex'), decode('00000000000000000000000000000004','hex'));

insert into test.Usr(id, contactId, parentTemplateId, managerId, tc, name, name$, pwd, update_count)
   values(decode('00000000000000000000000000000001','hex'), decode('00000000000000000000000000000001','hex'), decode('00000000000000000000000000000006','hex'), decode('00000000000000000000000000000002','hex'), 'U', 'jtest', upper('jtest'), 'secret', 0);
insert into test.Usr(id, contactId, parentTemplateId, managerId, tc, name, name$, pwd, update_count)
   values(decode('00000000000000000000000000000002','hex'), decode('00000000000000000000000000000003','hex'), decode('00000000000000000000000000000006','hex'), null, 'U', 'jsmith', upper('jsmith'), 'password', 0);
insert into test.Usr(id, contactId, managerId, tc, name, name$, pwd, update_count)
   values(decode('00000000000000000000000000000003','hex'), null, null, 'G', 'QA', upper('QA'), null, 0);
insert into test.Usr(id, contactId, managerId, tc, name, name$, pwd, update_count)
   values(decode('00000000000000000000000000000004','hex'), null, null, 'G', 'users', upper('users'), null, 0);
insert into test.Usr(id, contactId, managerId, tc, name, name$, pwd, update_count)
   values(decode('00000000000000000000000000000005','hex'), null, null, 'Y', 'SystemUser', upper('SystemUser'), null, 0);
insert into test.Usr(id, contactId, managerId, tc, name, name$, pwd, update_count)
   values(decode('00000000000000000000000000000006','hex'), null, null, 'T', 'Default', upper('Default'), null, 0);

insert into test.UserPassword(id, userId, passwordHash, activeFlag, createdDate)
   values(decode('00000000000000000000000000000001','hex'), decode('00000000000000000000000000000001','hex'), 'K7gNU3sdo+OL0wNhqoVWhr3g6s1xYv72ol/pe/Unols=', TRUE, null);
insert into test.UserPassword(id, userId, passwordHash, activeFlag, createdDate)
   values(decode('00000000000000000000000000000002','hex'), decode('00000000000000000000000000000002','hex'), 'XohImNooBHFR0OVvjcYpJ3NgPQ1qq73WKhHvch0VQtg=', TRUE, null);
insert into test.UserPassword(id, userId, passwordHash, activeFlag, createdDate)
   values(decode('00000000000000000000000000000003','hex'), decode('00000000000000000000000000000003','hex'), '', TRUE, null);
insert into test.UserPassword(id, userId, passwordHash, activeFlag, createdDate)
   values(decode('00000000000000000000000000000004','hex'), decode('00000000000000000000000000000004','hex'), '', TRUE, null);
insert into test.UserPassword(id, userId, passwordHash, activeFlag, createdDate)
   values(decode('00000000000000000000000000000005','hex'), decode('00000000000000000000000000000005','hex'), '', TRUE, null);
insert into test.UserPassword(id, userId, passwordHash, activeFlag, createdDate)
   values(decode('00000000000000000000000000000006','hex'), decode('00000000000000000000000000000006','hex'), '', TRUE, null);

insert into test.UsrPriv(id, userId, name)
   values(decode('00000000000000000000000000000001','hex'), decode('00000000000000000000000000000001','hex'), 'contactEntry');
insert into test.UsrPriv(id, userId, name)
   values(decode('00000000000000000000000000000002','hex'), decode('00000000000000000000000000000001','hex'), 'contactManagement');
insert into test.UsrPriv(id, userId, name)
   values(decode('00000000000000000000000000000003','hex'), decode('00000000000000000000000000000001','hex'), 'dummyRead');
insert into test.UsrPriv(id, userId, name)
   values(decode('00000000000000000000000000000004','hex'), decode('00000000000000000000000000000001','hex'), 'dummyUpdate');
insert into test.UsrPriv(id, userId, name)
   values(decode('00000000000000000000000000000005','hex'), decode('00000000000000000000000000000001','hex'), 'readAddress');
insert into test.UsrPriv(id, userId, name)
   values(decode('00000000000000000000000000000006','hex'), decode('00000000000000000000000000000001','hex'), 'addressContactRead');
insert into test.UsrPriv(id, userId, name)
   values(decode('00000000000000000000000000000007','hex'), decode('00000000000000000000000000000001','hex'), 'BatchJobRun');
insert into test.UsrPriv(id, userId, name)
   values(decode('00000000000000000000000000000008','hex'), decode('00000000000000000000000000000001','hex'), 'BatchJobManage');
insert into test.UsrPriv(id, userId, name)
   values(decode('00000000000000000000000000000009','hex'), decode('00000000000000000000000000000001','hex'), 'WorkflowManage');
insert into test.UsrPriv(id, userId, name)
   values(decode('0000000000000000000000000000000A','hex'), decode('00000000000000000000000000000001','hex'), 'updatePatient');
insert into test.UsrPriv(id, userId, name)
   values(decode('0000000000000000000000000000000B','hex'), decode('00000000000000000000000000000001','hex'), 'deletePatient');
insert into test.UsrPriv(id, userId, name)
   values(decode('0000000000000000000000000000000C','hex'), decode('00000000000000000000000000000001','hex'), 'childrenUpdate');

insert into test.Version(namespace, version, step, upgradable, test, loaded, locking) values ('test', '-1', -1, TRUE, TRUE, TRUE, 1);

insert into test.Queue(id, name, classCode, caption, locking, customized, system)
   values(decode('00000000000000000000000000000001','hex'), 'lifo', 'WFLIFOQueue', 'LIFO', 0, FALSE, TRUE);
insert into test.WFQueue(id)
   values(decode('00000000000000000000000000000001','hex'));
insert into test.Queue(id, name, classCode, caption, locking, customized, system)
   values(decode('00000000000000000000000000000002','hex'), 'fifo', 'WFFIFOQueue', 'FIFO', 0, FALSE, TRUE);
insert into test.WFQueue(id)
   values(decode('00000000000000000000000000000002','hex'));
insert into test.Queue(id, name, classCode, caption, locking, customized, system, concurrency, priority)
   values(decode('00000000000000000000000000000003','hex'), 'Semaphore', 'WFWindowedFIFOQueue', 'Semaphore', 0, FALSE, TRUE, 2, 10);
insert into test.WFQueue(id, schedulerId)
   values(decode('00000000000000000000000000000003','hex'), decode('00000000000000000000000000000001','hex'));
insert into test.Queue(id, name, classCode, caption, locking, customized, system, concurrency, priority)
   values(decode('00000000000000000000000000000004','hex'), 'SemaphoreLowConcurrency', 'WFWindowedFIFOQueue', 'SemaphoreLowConcurrency', 0, FALSE, TRUE, 1, 20);
insert into test.WFQueue(id, schedulerId)
   values(decode('00000000000000000000000000000004','hex'), decode('00000000000000000000000000000001','hex'));
insert into test.Queue(id, name, classCode, caption, locking, customized, system, concurrency, priority, throttleCounterId)
   values(decode('00000000000000000000000000000005','hex'), 'SemaphoreThrottled', 'WFWindowedFIFOQueue', 'SemaphoreThrottled', 0, FALSE, TRUE, NULL, 5, decode('00000000000000000000000000000001','hex'));
insert into test.WFQueue(id, schedulerId)
   values(decode('00000000000000000000000000000005','hex'), decode('00000000000000000000000000000001','hex'));
insert into test.Queue(id, classCode, name, caption, priority, customized, system, locking) 
   values (decode('00000000000000000000000000000006','hex'), 'ObjQueue', 'ObjectDispatcherQueue', 'idsc.rpc.queueing.dispatcherQueue', 0, FALSE, TRUE, 0);
insert into test.Queue (id, classCode, name, caption, priority, customized, system, locking) 
   values (decode('00000000000000000000000000000007','hex'), 'TestQueue', 'CommandRequestQueue', '', 0, FALSE, TRUE, 0);
insert into test.Queue (id, classCode, name, caption, priority, customized, system, locking) 
   values (decode('00000000000000000000000000000008','hex'), 'ObjQueue', 'SysMessageQueue', '', 0, FALSE, TRUE, 0);
insert into test.Queue (id, classCode, name, caption, priority, customized, system, locking) 
   values (decode('00000000000000000000000000000009','hex'), 'ObjQueue', 'SignalErrorQueue', '', 0, FALSE, TRUE, 0);
insert into test.Queue(id, classCode, name, caption, priority, customized, system, locking) 
   values (decode('0000000000000000000000000000000A','hex'), 'ObjQueue', 'ObjectSystemQueue', '', 0, FALSE, TRUE, 0);

insert into test.ObjectQueue (id, timeout, sendEnabled, receiveEnabled, errorCount) 
   values (decode('00000000000000000000000000000006','hex'), 6000, TRUE, TRUE, 1);
insert into test.ObjectQueue (id, timeout, sendEnabled, receiveEnabled, errorCount, errorQueueId) 
   values (decode('00000000000000000000000000000007','hex'), 6000, TRUE, TRUE, 2, decode('00000000000000000000000000000009','hex'));
insert into test.ObjectQueue (id, timeout, sendEnabled, receiveEnabled, errorCount, errorQueueId) 
   values (decode('00000000000000000000000000000008','hex'), 6000, TRUE, TRUE, 2, decode('00000000000000000000000000000009','hex'));
insert into test.ObjectQueue (id, timeout, sendEnabled, receiveEnabled, errorCount) 
   values (decode('00000000000000000000000000000009','hex'), 6000, TRUE, TRUE, 1);
insert into test.ObjectQueue (id, timeout, sendEnabled, receiveEnabled, errorCount) 
   values (decode('0000000000000000000000000000000A','hex'), 6000, TRUE, TRUE, 1);

insert into test.ThrottleCounter(id, cnt, rate, batchJobId, locking)
   values(decode('00000000000000000000000000000001','hex'), 2, 2, decode('00000000000000000000000000000002','hex'), 0);

insert into test.ObjectQueueDispatcher (id) 
   values (decode('00000000000000000000000000000001','hex'));

-- Same as BatchJob id
insert into test.WFSchedulerLock(id)
   values(decode('00000000000000000000000000000001','hex'));

insert into test.SysHistory(id, createdTime, principalId, caption, caption$, idKey, idValue, data, contextState, wkspMemento, sourcePortletPath, locking)
   values(decode('00000000000000000000000000000001','hex'), to_timestamp(1), decode('00000000000000000000000000000001','hex'), 'TestPortalAPIPortlet$apiTestFlat100E012A4FEE3F4044BA495CD0A0BAA06C', upper('TestPortalAPIPortlet$apiTestFlat100E012A4FEE3F4044BA495CD0A0BAA06C'), 'history', 'TestA', '3vPP4YdataUPP7Ycaption21sDocked WorkspaceAPI 1PP4YiconUPP12YwkspCtxtName10scontextVarPP11YwkspCtxtVal4svalAU', '3vPP9sMyContext66sTestPortalAPIPortlet$apiTestFlat10ABA9D0761FC9414093612E90B44B14D7PP10scontextVar4svalAU', '3v3T1s$U1I0U5sview$3T1s$U1I0U13srootLayoutAPI1T1s$U1I0U7sweightsP3s50%P3s49%U46sapiTestFlat$10ABA9D0761FC9414093612E90B44B14D72T1s$U1I0U8swndState1I419sexternalMementoData170s3v3T1S$U1I2U6Smodel$U15SlastAccessTime$13L13002431005915Sview$1T1#U1I0U88STestPortalAPIPortlet..TestPortalAPIPortlet$apiTestFlat10ABA9D0761FC9414093612E90B44B14D7T1#U1I0U46sapiTestRich$10ABA9D0761FC9414093612E90B44B14D71T1s$U1I0U8swndState1I446sapiTestRich$10ABA9D0761FC9414093612E90B44B14D71T1s$U1I0U15slastAccessTime$13L130024310438115slastAccessTime$13L1300243104381', 'WorkspaceAPI$100E012A4FEE3F4044BA495CD0A0BAA06C apiTestFlat$100E012A4FEE3F4044BA495CD0A0BAA06C', 0);
insert into test.SysHistory(id, createdTime, principalId, caption, caption$, idKey, idValue, data, contextState, wkspMemento, sourcePortletPath, locking)
   values(decode('00000000000000000000000000000002','hex'), to_timestamp(2), decode('00000000000000000000000000000001','hex'), 'TestPortalAPIPortlet$apiTestFlat103D020108ED2143CA8A08FC41F57B0CB1', upper('TestPortalAPIPortlet$apiTestFlat100E012A4FEE3F4044BA495CD0A0BAA06C'), 'history', 'TestB', '3vPP4YdataUPP7Ycaption21sDocked WorkspaceAPI 2PP4YiconUPP12YwkspCtxtName10scontextVarPP11YwkspCtxtVal4svalBU', '3vPP9sMyContext66sTestPortalAPIPortlet$apiTestFlat10EB62D7C67F184900AD8929D9F4D5E41DPP10scontextVar4svalBU', '3v3T1s$U1I0U5sview$3T1s$U1I0U13srootLayoutAPI1T1s$U1I0U7sweightsP3s50%P3s49%U46sapiTestFlat$10EB62D7C67F184900AD8929D9F4D5E41D2T1s$U1I0U8swndState1I419sexternalMementoData170s3v3T1S$U1I2U6Smodel$U15SlastAccessTime$13L13002431007025Sview$1T1#U1I0U88STestPortalAPIPortlet..TestPortalAPIPortlet$apiTestFlat10EB62D7C67F184900AD8929D9F4D5E41DT1#U1I0U46sapiTestRich$10EB62D7C67F184900AD8929D9F4D5E41D1T1s$U1I0U8swndState1I446sapiTestRich$10EB62D7C67F184900AD8929D9F4D5E41D1T1s$U1I0U15slastAccessTime$13L130024326824115slastAccessTime$13L1300243268241', 'WorkspaceAPI$103D020108ED2143CA8A08FC41F57B0CB1 apiTestFlat$103D020108ED2143CA8A08FC41F57B0CB1', 0);
