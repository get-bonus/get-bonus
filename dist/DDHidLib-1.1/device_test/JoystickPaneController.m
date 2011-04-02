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

#import "JoystickPaneController.h"
#import "DDHidJoystick.h"
#import "ButtonState.h"

@interface JoystickPaneController (Private)

- (void) setJoysticks: (NSArray *) theJoysticks;

@end

@implementation JoystickPaneController

- (void) awakeFromNib;
{
    NSArray * joysticks = [DDHidJoystick allJoysticks];

    mJoystickButtons = [[NSMutableArray alloc] init];
    [joysticks makeObjectsPerformSelector: @selector(setDelegate:)
                               withObject: self];
    [self setJoysticks: joysticks];
    if ([mJoysticks count] > 0)
        [self setJoystickIndex: 0];
    else
        [self setJoystickIndex: NSNotFound];
}

//=========================================================== 
//  joysticks 
//=========================================================== 
- (NSArray *) joysticks
{
    return mJoysticks; 
}

- (NSArray *) joystickButtons;
{
    return mJoystickButtons;
}

//=========================================================== 
//  joystickIndex 
//=========================================================== 
- (unsigned) joystickIndex
{
    return mJoystickIndex;
}

- (void) setJoystickIndex: (unsigned) theJoystickIndex
{
    if (mCurrentJoystick != nil)
    {
        [mCurrentJoystick stopListening];
        mCurrentJoystick = nil;
    }
    mJoystickIndex = theJoystickIndex;
    [mJoysticksController setSelectionIndex: mJoystickIndex];
    if (mJoystickIndex != NSNotFound)
    {
        mCurrentJoystick = [mJoysticks objectAtIndex: mJoystickIndex];
        [mCurrentJoystick startListening];
        
        [self willChangeValueForKey: @"joystickButtons"];
        [mJoystickButtons removeAllObjects];
        NSArray * buttons = [mCurrentJoystick buttonElements];
        NSEnumerator * e = [buttons objectEnumerator];
        DDHidElement * element;
        while (element = [e nextObject])
        {
            ButtonState * state = [[ButtonState alloc] initWithName: [[element usage] usageName]];
            [state autorelease];
            [mJoystickButtons addObject: state];
        }
        [self didChangeValueForKey: @"joystickButtons"];
    }
}

- (int) xAxis;
{
    return mXAxis;
}

- (int) yAxis;
{
    return mYAxis;
}

- (void) ddhidJoystick: (DDHidJoystick *)  joystick
                 stick: (unsigned) stick
              xChanged: (int) value;
{
    [self willChangeValueForKey: @"xAxis"];
    mXAxis = value;
    [self didChangeValueForKey: @"xAxis"];
}

- (void) ddhidJoystick: (DDHidJoystick *)  joystick
                 stick: (unsigned) stick
              yChanged: (int) value;
{
    [self willChangeValueForKey: @"yAxis"];
    mYAxis = value;
    [self didChangeValueForKey: @"yAxis"];
}

- (void) ddhidJoystick: (DDHidJoystick *) joystick
                 stick: (unsigned) stick
             otherAxis: (unsigned) otherAxis
          valueChanged: (int) value;
{
    // Somehow display values here
    NSLog(@"Stick: %d, other axis: %d, changed: %d", stick, otherAxis, value);
}

- (void) ddhidJoystick: (DDHidJoystick *) joystick
                 stick: (unsigned) stick
             povNumber: (unsigned) povNumber
          valueChanged: (int) value;
{
    // Somehow display values here
    NSLog(@"Stick: %d, POV number: %d, changed: %d", stick, povNumber, value);
}

- (void) ddhidJoystick: (DDHidJoystick *) joystick
            buttonDown: (unsigned) buttonNumber;
{
    ButtonState * state = [mJoystickButtons objectAtIndex: buttonNumber];
    [state setPressed: YES];
}

- (void) ddhidJoystick: (DDHidJoystick *) joystick
              buttonUp: (unsigned) buttonNumber;
{
    ButtonState * state = [mJoystickButtons objectAtIndex: buttonNumber];
    [state setPressed: NO];
}

@end

@implementation JoystickPaneController (Private)

- (void) setJoysticks: (NSArray *) theJoysticks
{
    if (mJoysticks != theJoysticks)
    {
        [mJoysticks release];
        mJoysticks = [theJoysticks retain];
    }
}

@end
