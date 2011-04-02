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

// Based on Martin Kahr's Apple Remote wrapper
// http://www.martinkahr.com/source-code/

#import "RemoteFeedbackView.h"

@implementation RemoteFeedbackView

- (id)initWithFrame:(NSRect)frameRect
{
	if ((self = [super initWithFrame:frameRect]) != nil) {
		remoteImage = [[NSImage imageNamed:@"AppleRemote"] retain];
		lastButtonIdentifier = -1;
	}
	return self;
}

- (void) dealloc {

	[remoteImage release];
	[super dealloc];
}

- (void) clearAfterRedraw: (id) sender 
{
	clearAfterDraw = NO;		
	[self ddhidAppleRemoteButton: lastButtonIdentifier pressedDown: NO];	
}

- (void) ddhidAppleRemoteButton: (DDHidAppleRemoteEventIdentifier)buttonIdentifier 
                    pressedDown: (BOOL) pressedDown;
{
	
	clearAfterDraw = NO;
	
	if (pressedDown) {
		lastButtonIdentifier = buttonIdentifier;
	} else {
		if (drawn) 
			lastButtonIdentifier = -1;
		else {			
			lastButtonIdentifier = buttonIdentifier;
			clearAfterDraw = YES;
		}
	}
		
	drawn = NO;
	[self setNeedsDisplay:YES];
}

- (void)drawRect:(NSRect)rect
{
	drawn = YES;
	NSRect imageRect;
	NSRect drawingRect;
	imageRect.origin = NSZeroPoint;
	imageRect.size   = [remoteImage size];
	
	int x = ([self bounds].size.width  - [remoteImage size].width)/2;
	int y = ([self bounds].size.height - [remoteImage size].height)/2;
	
	drawingRect.origin = NSMakePoint(x, y);
	drawingRect.size   = imageRect.size;
	
	[remoteImage drawInRect: drawingRect
				   fromRect: imageRect
				  operation: NSCompositeSourceOver
				   fraction: 1.0];
	
	
	if (lastButtonIdentifier == -1) {
		return;
	}
	
	DDHidAppleRemoteEventIdentifier buttonToSelect = lastButtonIdentifier;
	
	NSPoint buttonPos;
	float opacity = 0.5;
	
	switch(buttonToSelect) {
		case kDDHidRemoteButtonVolume_Plus:
			buttonPos.x = 53;
			buttonPos.y = 240;
			break;
		case kDDHidRemoteButtonVolume_Minus:
			buttonPos.x = 53;
			buttonPos.y = 180;			
			break;
		case kDDHidRemoteButtonMenu_Hold:
			opacity = 0.8;			
		case kDDHidRemoteButtonMenu:
			buttonPos.x = 53;
			buttonPos.y = 137;
			clearAfterDraw = YES;
			break;			
		case kDDHidRemoteButtonPlay_Sleep:
			opacity = 0.8;
		case kDDHidRemoteButtonPlay:
			buttonPos.x = 53;
			buttonPos.y = 210;
			clearAfterDraw = YES;			
			break;
		case kDDHidRemoteButtonRight_Hold:
			opacity = 0.8;
			buttonPos.x = 83;
			buttonPos.y = 210;	
			break;
		case kDDHidRemoteButtonRight:
			buttonPos.x = 83;
			buttonPos.y = 210;	
			clearAfterDraw = YES;
			break;
		case kDDHidRemoteButtonLeft_Hold:
			opacity = 0.8;			
			buttonPos.x = 23;
			buttonPos.y = 210;
			break;
		case kDDHidRemoteButtonLeft:
			buttonPos.x = 23;
			buttonPos.y = 210;
			clearAfterDraw = YES;			
			break;
		default:
			break;
	}
	
	float width = 20.0;
	float height= 20.0;
    NSRect r = NSMakeRect(buttonPos.x + x - (width/2), buttonPos.y + y - (height/2), width, height);
    NSBezierPath* bp = [NSBezierPath bezierPathWithOvalInRect:r];
	
	[[NSColor colorWithCalibratedRed:1.0 green:0.0 blue:0.0 alpha:opacity] set];
    [bp fill];	
	if (clearAfterDraw) {
		[self performSelector:@selector(clearAfterRedraw:) withObject:self afterDelay:0.1];
	}
}

@end
