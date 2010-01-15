//
//  ErlyVideo.m
//  ErlyVideo
//
//  Created by sK0T on 15.01.10.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import "ErlyVideo.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

@implementation ErlyVideo

static BOOL FakeStatus = NO;

+ (void)load
{
	erl_init();
}

+ (BOOL)isActive
{
	struct sockaddr_in address;
	int epmd;
	
	
	epmd = socket(AF_INET, SOCK_STREAM, 0);
	if (epmd < 0) return NO;
	
	address.sin_family = AF_INET;
	address.sin_addr.s_addr = inet_addr ("127.0.0.1");
	address.sin_port = htons(4369);
	
	if (connect(epmd, &address, sizeof(address)) < 0) {
		return NO;
	}
	
	//write(epmd, 
	
	return FakeStatus;
}

+ (BOOL)installed
{
	return [[NSFileManager defaultManager] fileExistsAtPath:@"/Library/ErlyVideo/ebin/erlmedia.app"];
}

+ (int)port
{
	return 1935;
}

+ (void)stop:(id<ErlyVideoClient>)sender
{
	FakeStatus = NO;
	[(NSObject *)sender performSelector:@selector(erlyVideoStatusChanged)
							 withObject:nil
							 afterDelay:1.0];
}

+ (void)start:(id<ErlyVideoClient>)sender
{
	FakeStatus = YES;
	[(NSObject *)sender performSelector:@selector(erlyVideoStatusChanged)
							 withObject:nil
							 afterDelay:1.0];
	NSTask *task = [[NSTask alloc] init];
	[task setLaunchPath:@"/usr/bin/make"];
	[task setArguments:[NSArray arrayWithObjects:@"-f", @"/Library/ErlyVideo", @"start", nil];
	[task launch];
	
}

@end
