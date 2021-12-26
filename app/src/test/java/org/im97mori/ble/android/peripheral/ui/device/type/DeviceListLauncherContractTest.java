package org.im97mori.ble.android.peripheral.ui.device.type;

import static androidx.test.espresso.intent.matcher.IntentMatchers.hasComponent;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_UNDEFINED;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_TYPE;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class DeviceListLauncherContractTest {

    @Rule
    public HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Inject
    @ApplicationContext
    Context mContext;

    private final DeviceListLauncherContract mDeviceListLauncherContract = new DeviceListLauncherContract();

    @Before
    public void setUp() {
        mHiltRule.inject();
    }

    @Test
    public void test_createIntent_00001() {
        Intent intent = mDeviceListLauncherContract.createIntent(mContext, null);
        assertTrue(hasComponent(new ComponentName(mContext, DeviceTypeListActivity.class)).matches(intent));
    }

    @Test
    public void test_parseResult_00001() {
        assertNull(mDeviceListLauncherContract.parseResult(Activity.RESULT_OK, null));
    }

    @Test
    public void test_parseResult_00002() {
        Integer integer = mDeviceListLauncherContract.parseResult(Activity.RESULT_OK, new Intent());
        assertNotNull(integer);
        assertEquals(DEVICE_TYPE_UNDEFINED, integer.intValue());
    }

    @Test
    public void test_parseResult_00003() {
        Intent intent = new Intent();
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        Integer integer = mDeviceListLauncherContract.parseResult(Activity.RESULT_OK, intent);
        assertNotNull(integer);
        assertEquals(DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, integer.intValue());
    }

    @Test
    public void test_parseResult_00004() {
        assertNull(mDeviceListLauncherContract.parseResult(Activity.RESULT_CANCELED, null));
    }

}