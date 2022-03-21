package org.im97mori.ble.android.peripheral.room;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNull;

import static org.junit.Assert.assertArrayEquals;

import android.os.Build;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class DeviceSettingTest {

    @Test
    public void test_constructor_00001() {
        long id = 1;
        DeviceSetting deviceSetting = new DeviceSetting(id);

        assertEquals(id, deviceSetting.getId());
        assertEquals("", deviceSetting.getDeviceSettingName());
        assertEquals(0, deviceSetting.getDeviceType());
        assertNull(deviceSetting.getDeviceSettingData());
    }

    @Test
    public void test_constructor_00002() {
        String deviceSettingName = "a";
        int deviceType = 1;
        DeviceSetting deviceSetting = new DeviceSetting(deviceSettingName, deviceType);

        assertEquals(0, deviceSetting.getId());
        assertEquals(deviceSettingName, deviceSetting.getDeviceSettingName());
        assertEquals(deviceType, deviceSetting.getDeviceType());
        assertNull(deviceSetting.getDeviceSettingData());
    }

    @Test
    public void test_constructor_00003() {
        long id = 1;
        String deviceSettingName = "a";
        int deviceType = 2;
        byte[] deviceSettingData = new byte[]{1};
        DeviceSetting deviceSetting = new DeviceSetting(id, deviceSettingName, deviceType, deviceSettingData);

        assertEquals(id, deviceSetting.getId());
        assertEquals(deviceSettingName, deviceSetting.getDeviceSettingName());
        assertEquals(deviceType, deviceSetting.getDeviceType());
        assertArrayEquals(deviceSettingData, deviceSetting.getDeviceSettingData());
    }

    @Test
    public void test_constructor_00004() {
        int deviceType = 1;
        DeviceSetting deviceSetting = new DeviceSetting(deviceType);

        assertEquals(0, deviceSetting.getId());
        assertEquals("", deviceSetting.getDeviceSettingName());
        assertEquals(deviceType, deviceSetting.getDeviceType());
        assertNull(deviceSetting.getDeviceSettingData());
    }

}