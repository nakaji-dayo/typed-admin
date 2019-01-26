create table beer (
  id bigint not null,
  name varchar(255) not null,
  ibu int,
  release_date date,
  primary key ("id")
);

create table store (
  id bigint not null,
  name varchar(255) not null,
  addr text,
  primary key ("id")
);

create table comment (
  id bigint not null,
  beer_id bigint references beer(id) not null,
  body varchar(255) not null,
  primary key ("id")
);
