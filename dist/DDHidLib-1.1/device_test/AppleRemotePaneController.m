/*
 * Copyright (c) 2007 Dave Dribin
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#import "AppleRemotePaneController.h"
#import "RemoteFeedbackView.h"

@implementation AppleRemotePaneController

- (void) awakeFromNib;
{
    [self willChangeValueForKey: @"remote"];
    mRemote = [[DDHidAppleRemote firstRemote] retain];
    [self didChangeValueForKey: @"remote"];
    
    [mRemote setDelegate: self];
    [self setOpenInExclusiveMode: YES];
}

//=========================================================== 
// dealloc
//=========================================================== 
- (void) dealloc
{
    [mRemote release];
    
    mRemote = nil;
    [super dealloc];
}

- (DDHidAppleRemote *) remote;
{
    return mRemote;
}

- (IBAction) toggleListening: (id) sender;
{
    NSAssert(mRemote != nil, @"Remote is nil");
    
    if ([mRemote isListening])
    {
        [mRemote stopListening];
        [mStartStopButton setTitle: @"Start Listening"];
    }
    else
    {
        [mRemote setListenInExclusiveMode: mOpenInExclusiveMode];
        [mRemote startListening];
        [mStartStopButton setTitle: @"Stop Listening"];
    }
}

//=========================================================== 
//  openInExclusiveMode 
//=========================================================== 
- (BOOL) openInExclusiveMode
{
    return mOpenInExclusiveMode;
}

- (void) setOpenInExclusiveMode: (BOOL) flag
{
    mOpenInExclusiveMode = flag;
}

- (void) ddhidAppleRemoteButton: (DDHidAppleRemoteEventIdentifier) buttonIdentifier
                    pressedDown: (BOOL) pressedDown;
{
	NSString * buttonName= nil;
	NSString * pressed = @"";
	
	switch(buttonIdentifier)
    {
		case kDDHidRemoteButtonVolume_Plus:
			buttonName = @"Volume up";
			if (pressedDown) pressed = @"(down)"; else pressed = @"(up)";
			break;
		case kDDHidRemoteButtonVolume_Minus:
			buttonName = @"Volume down";
			if (pressedDown) pressed = @"(down)"; else pressed = @"(up)";
			break;			
		case kDDHidRemoteButtonMenu:
			buttonName = @"Menu";
			break;			
		case kDDHidRemoteButtonPlay:
			buttonName = @"Play";
			break;			
		case kDDHidRemoteButtonRight:	
			buttonName = @"Right";
			break;			
		case kDDHidRemoteButtonLeft:
			buttonName = @"Left";
			break;			
		case kDDHidRemoteButtonRight_Hold:
			buttonName = @"Right holding";	
			if (pressedDown) pressed = @"(down)"; else pressed = @"(up)";
			break;	
		case kDDHidRemoteButtonLeft_Hold:
			buttonName = @"Left holding";		
			if (pressedDown) pressed = @"(down)"; else pressed = @"(up)";
			break;			
		case kDDHidRemoteButtonPlay_Sleep:
			buttonName = @"Play (sleep mode)";
			break;			
		case kDDHidRemoteButtonMenu_Hold:
			buttonName = @"Menu (long)";
			break;
		case kDDHidRemoteControl_Switched:
			buttonName = @"Remote Control Switched";
			break;
        case kDDHidRemoteControl_Paired:
            buttonName = @"Remote Control Paired";
            break;
		default:
			NSLog(@"Unmapped event for button %d", buttonIdentifier); 
			break;
	}	
	[mFeedbackText setStringValue:[NSString stringWithFormat:@"%@ %@",
        buttonName, pressed]];

    [mFeedbackView ddhidAppleRemoteButton: buttonIdentifier
                              pressedDown: pressedDown];
}

@end
