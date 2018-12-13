
create table beer (
  id bigint not null,
  name varchar(255) not null,
  ibu int,
  primary key ("id")
);

create table beer_image (
  id bigint not null,
  beer_id bigint references beer(id) not null,
  filename text,
  primary key ("id")
);

create table store (
  id bigint not null,
  name varchar(255) not null,
  addr text,
  primary key ("id")
);
