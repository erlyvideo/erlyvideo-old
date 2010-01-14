//
//  ErlyVideoPref.h
//  ErlyVideo
//
//  Created by sK0T on 13.01.10.
//  Copyright (c) 2010 __MyCompanyName__. All rights reserved.
//

#import <PreferencePanes/PreferencePanes.h>


@interface ErlyVideoPref : NSPreferencePane 
{
	IBOutlet NSButton *enableServer;
}

- (IBAction)enableServerClicked:(NSButton *)sender;
- (IBAction)help:(NSButton *)sender;
- (void)mainViewDidLoad;

@end
