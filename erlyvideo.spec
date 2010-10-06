Summary: Erlyvideo multiprotocol streaming server
Name: erlyvideo
Version: 2.4.4
Release: stable
License: GPL
Group: Network
BuildRoot: /tmp/erlyvideo-%{version}
Source: http://debian.erlyvideo.org/tgz/erlyvideo-%{version}.tar.gz
URL: http://erlyvideo.org/
BuildRequires: erlang, ruby
Requires: erlang


%description 
Erlyvideo is a multiprotocol opensource videostreaming server
written with efficiency in mind

%prep
%setup -n erlyvideo-%{version}


%build
%configure
make


%install
mkdir -p $RPM_BUILD_ROOT
make DESTROOT=$RPM_BUILD_ROOT install


%files
/usr/bin/*
%config /etc/*
%doc README.md


%clean
rm -rf $RPM_BUILD_ROOT
