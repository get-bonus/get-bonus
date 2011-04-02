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

#import "HidBrowserController.h"
#import "DDHidUsageTables.h"
#import "DDHidDevice.h"
#import "DDHidElement.h"
#import "WatcherWindowController.h"

@implementation HidBrowserController

static BOOL sSleepAtExit = NO;

static void exit_sleeper()
{
    while (sSleepAtExit) sleep(60);
}

- (void) awakeFromNib
{
    sSleepAtExit = [[NSUserDefaults standardUserDefaults] boolForKey: @"SleepAtExit"];
    atexit(exit_sleeper);

    [self willChangeValueForKey: @"devices"];
    mDevices = [[DDHidDevice allDevices] retain];
    [self didChangeValueForKey: @"devices"];
    
    [mWindow center];
    [mWindow makeKeyAndOrderFront: self];
}

//=========================================================== 
// dealloc
//=========================================================== 
- (void) dealloc
{
    [mDevices release];
    
    mDevices = nil;
    [super dealloc];
}

//=========================================================== 
// - devices
//=========================================================== 
- (NSArray *) devices
{
    return mDevices; 
}

- (DDHidDevice *) selectedDevice;
{
    NSArray * selectedDevices = [mDevicesController selectedObjects];
    if ([selectedDevices count] > 0)
        return [selectedDevices objectAtIndex: 0];
    else
        return nil;
}

- (IBAction) watchSelected: (id) sender;
{
    NSArray * selectedElements = [mElementsController selectedObjects];
    if ([selectedElements count] == 0)
        return;

    WatcherWindowController * controller =
        [[WatcherWindowController alloc] init];
    [controller setDevice: [self selectedDevice]];
    [controller setElements: selectedElements];
    [controller showWindow: self];
}

- (IBAction) exportPlist: (id) sender;
{
    DDHidDevice * selectedDevice = [self selectedDevice];
    if (selectedDevice == nil)
        return;

    NSSavePanel * panel = [NSSavePanel savePanel];
    
    /* set up new attributes */
    [panel setRequiredFileType: @"plist"];
    [panel setAllowsOtherFileTypes: NO];
    [panel setCanSelectHiddenExtension: YES];
    
    /* display the NSSavePanel */
    [panel beginSheetForDirectory: NSHomeDirectory()
                             file: @""
                   modalForWindow: [NSApp mainWindow]
                    modalDelegate: self
                   didEndSelector: @selector(exportPlistPanelDidEnd:returnCode:contextInfo:)
                      contextInfo: selectedDevice];
}

- (void) exportPlistPanelDidEnd: (NSSavePanel *) panel
                     returnCode: (int) returnCode
                    contextInfo: (void *) contextInfo;
{
    DDHidDevice * selectedDevice = contextInfo;

    /* if successful, save file under designated name */
    if (returnCode != NSOKButton)
        return;
    
    NSDictionary * deviceProperties = [selectedDevice properties];
    if (![deviceProperties writeToFile: [panel filename] atomically: YES])
        NSBeep();
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication
{
    return YES;
}

- (void)applicationWillTerminate:(NSNotification *)aNotification
{
    [self willChangeValueForKey: @"devices"];
    [mDevices release];
    mDevices = nil;
    [self didChangeValueForKey: @"devices"];
}

@end
