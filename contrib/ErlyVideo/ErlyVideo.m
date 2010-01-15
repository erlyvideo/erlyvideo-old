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

enum {
	EPMD_ALIVE2_REQ = 120,
	EPMD_PORT_PLEASE2_REQ = 122,
	EPMD_NAMES_REQ = 110,
	EPMD_DUMP_REQ = 100,
	EPMD_KILL_REQ = 107,
	EPMD_STOP_REQ = 115,
	EPMD_ALIVE2_RESP = 121,
	EPMD_PORT2_RESP = 119,
	EPMD_NODETYPE_HIDDEN = 72,
	EPMD_NODETYPE_NORMAL = 77,
	
	EPMD_PROTOCOL = 0,
	
	EPMD_VERSION_LOW = 5,
	EPMD_VERSION_HIGH = 5,
	EPMD_PORT = 4369
};

@implementation ErlyVideo

static BOOL FakeStatus = NO;
+ (NSArray *)searchPathes
{
	return [NSArray arrayWithObjects:@"~/Library/ErlyVideo/",
			@"/Library/ErlyVideo/", nil];
}


+ (BOOL)isActive
{
	struct sockaddr_in address;
	int epmd;
	uint16_t length;
	uint8_t reply[256];
	char nodeName[] = "ems";
	
	epmd = socket(AF_INET, SOCK_STREAM, 0);
	if (epmd < 0) return NO;
	
	address.sin_family = AF_INET;
	address.sin_addr.s_addr = inet_addr ("127.0.0.1");
	address.sin_port = htons(EPMD_PORT);
	
	if (connect(epmd, (struct sockaddr *)&address, sizeof(address)) < 0) {
		return NO;
	}
	
	memset(reply, 0, sizeof(reply));
	length = strlen(nodeName);
	*(uint16_t *)reply = htons(length+1);
	reply[2] = EPMD_PORT_PLEASE2_REQ;
	memcpy(reply+3, nodeName, length);
	write(epmd, reply, length+3);
	
	if(read(epmd, reply, 4) != 4) {
		close(epmd);
		return NO;
	}
	close(epmd);

	if(reply[1]) {
		// No erlyvideo running
		return NO;
	}
	
	return YES;
}

+ (BOOL)installed
{
	NSFileManager *fMan = [NSFileManager defaultManager];
	for (NSString *path in [self searchPathes]) {
		NSString *put = [path stringByExpandingTildeInPath];
		BOOL dir = NO;

		if ([fMan fileExistsAtPath:put isDirectory:&dir]) {
			if (dir) {
				return YES;
			}
		}
	}
	return NO;
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
	[task setArguments:[NSArray arrayWithObjects:@"-f", @"/Library/ErlyVideo", @"start", nil]];
	[task launch];
	
}

@end
