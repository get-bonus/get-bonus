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

#import "KeyboardPaneController.h"
#import "DDHidLib.h"
#include <IOKit/hid/IOHIDUsageTables.h>

@interface KeyboardPaneController (Private)

- (void) addEvent: (NSString *) event usageId: (unsigned) usageId;

@end

@implementation KeyboardPaneController

- (id) init;
{
    self = [super init];
    if (self == nil)
        return nil;
    
    mEvents = [[NSMutableArray alloc] init];
    
    return self;
}

- (void) awakeFromNib;
{
    NSArray * keyboards = [DDHidKeyboard allKeyboards];
    [keyboards makeObjectsPerformSelector: @selector(setDelegate:)
                               withObject: self];
    [self setKeyboards: keyboards];
    
    if ([keyboards count] > 0)
        [self setKeyboardIndex: 0];
    else
        [self setKeyboardIndex: NSNotFound];
}

//=========================================================== 
// dealloc
//=========================================================== 
- (void) dealloc
{
    [mKeyboards release];
    [mEvents release];
    
    mKeyboards = nil;
    mEvents = nil;
    [super dealloc];
}

//=========================================================== 
//  keyboards 
//=========================================================== 
- (NSArray *) keyboards
{
    return mKeyboards; 
}

- (void) setKeyboards: (NSArray *) theKeyboards
{
    if (mKeyboards != theKeyboards)
    {
        [mKeyboards release];
        mKeyboards = [theKeyboards retain];
    }
}
//=========================================================== 
//  keyboardIndex 
//=========================================================== 
- (unsigned) keyboardIndex
{
    return mKeyboardIndex;
}

- (void) setKeyboardIndex: (unsigned) theKeyboardIndex
{
    if (mCurrentKeyboard != nil)
    {
        [mCurrentKeyboard stopListening];
        mCurrentKeyboard = nil;
    }
    mKeyboardIndex = theKeyboardIndex;
    [mKeyboardsController setSelectionIndex: mKeyboardIndex];
    [self willChangeValueForKey: @"events"];
    [mEvents removeAllObjects];
    [self didChangeValueForKey: @"events"];
    if (mKeyboardIndex != NSNotFound)
    {
        mCurrentKeyboard = [mKeyboards objectAtIndex: mKeyboardIndex];
        [mCurrentKeyboard startListening];
    }
}

//=========================================================== 
//  events 
//=========================================================== 
- (NSMutableArray *) events
{
    return mEvents; 
}

- (void) setEvents: (NSMutableArray *) theEvents
{
    if (mEvents != theEvents)
    {
        [mEvents release];
        mEvents = [theEvents retain];
    }
}
- (void) addEvent: (id)theEvent
{
    [[self events] addObject: theEvent];
}
- (void) removeEvent: (id)theEvent
{
    [[self events] removeObject: theEvent];
}


@end

@implementation KeyboardPaneController (DDHidKeyboardDelegate)

- (void) ddhidKeyboard: (DDHidKeyboard *) keyboard
               keyDown: (unsigned) usageId;
{
    [self addEvent: @"Key Down" usageId: usageId];
}

- (void) ddhidKeyboard: (DDHidKeyboard *) keyboard
                 keyUp: (unsigned) usageId;
{
    [self addEvent: @"Key Up" usageId: usageId];
}

@end

@implementation KeyboardPaneController (Private)

- (void) addEvent: (NSString *) event usageId: (unsigned) usageId;
{
    DDHidUsageTables * usageTables = [DDHidUsageTables standardUsageTables];
    NSString * description = [NSString stringWithFormat: @"%@ (0x%04X)",
        [usageTables descriptionForUsagePage: kHIDPage_KeyboardOrKeypad
                                       usage: usageId],
        usageId];
    
    NSMutableDictionary * row = [mKeyboardEventsController newObject];
    [row setObject: event forKey: @"event"];
    [row setObject: description forKey: @"description"];
    [mKeyboardEventsController addObject: row];
}

@end

