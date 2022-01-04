package org.im97mori.ble.android.peripheral.ui.device.setting;

import static androidx.test.espresso.intent.matcher.IntentMatchers.hasComponent;
import static androidx.test.espresso.intent.matcher.IntentMatchers.hasExtra;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_ID;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_TYPE;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.core.util.Pair;

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
public class DeviceSettingLauncherContractTest {

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Inject
    @ApplicationContext
    Context mContext;

    private final DeviceSettingLauncherContract mDeviceSettingLauncherContract = new DeviceSettingLauncherContract();

    @Before
    public void setUp() {
        mHiltRule.inject();
    }

    @Test
    public void test_createIntent_00001() {
        Pair<Long, Integer> pair = Pair.create(1L, 2);
        Intent intent = mDeviceSettingLauncherContract.createIntent(mContext, pair);
        assertTrue(hasComponent(new ComponentName(mContext, DeviceSettingActivity.class)).matches(intent));
        assertTrue(hasExtra(KEY_DEVICE_ID, pair.first).matches(intent));
        assertTrue(hasExtra(KEY_DEVICE_TYPE, pair.second).matches(intent));
    }

    @Test
    public void test_parseResult_00001() {
        assertTrue(mDeviceSettingLauncherContract.parseResult(Activity.RESULT_OK, null));
    }

    @Test
    public void test_parseResult_00002() {
        assertFalse(mDeviceSettingLauncherContract.parseResult(Activity.RESULT_CANCELED, null));
    }

}