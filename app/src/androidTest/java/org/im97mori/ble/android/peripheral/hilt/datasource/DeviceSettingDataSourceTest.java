package org.im97mori.ble.android.peripheral.hilt.datasource;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;

import android.content.Context;

import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import java.io.File;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import io.reactivex.rxjava3.disposables.CompositeDisposable;

@HiltAndroidTest
public class DeviceSettingDataSourceTest {

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    private final CompositeDisposable mDisposable = new CompositeDisposable();

    private DeviceSettingDataSource mDeviceSettingDataSource;

    private File mDatabaseFile;

    @Inject
    @ApplicationContext
    Context mContext;


    @Before
    public void setUp() {
        mHiltRule.inject();
        mDatabaseFile = mContext.getDatabasePath("app_database.db");
        if (mDatabaseFile.exists()) {
            assertTrue(mDatabaseFile.delete());
        }
        assertFalse(mDatabaseFile.exists());
        mDeviceSettingDataSource = new DeviceSettingDataSource(mContext);
    }

    @After
    public void tearDown() {
        mDisposable.clear();
    }

    @Test
    public void test_initDatabase_00001() {
        CountDownLatch countDownLatch = new CountDownLatch(1);
        mDisposable.add(mDeviceSettingDataSource.deleteAllDeviceSetting().subscribe(countDownLatch::countDown));

        boolean result = false;
        do {
            try {
                result = countDownLatch.await(1, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        } while (!result);

        assertTrue(mDatabaseFile.exists());
    }

    @Test
    public void test_loadAllDeviceSetting_00001() {
        List<DeviceSetting> deviceSettingList = mDeviceSettingDataSource.loadAllDeviceSetting().blockingGet();
        assertTrue(deviceSettingList.isEmpty());
    }

    @Test
    public void test_loadAllDeviceSetting_00002() {
        DeviceSetting original = new DeviceSetting("a", 1);
        mDeviceSettingDataSource.insertDeviceSetting(original).blockingSubscribe();

        List<DeviceSetting> deviceSettingList = mDeviceSettingDataSource.loadAllDeviceSetting().blockingGet();
        assertEquals(1, deviceSettingList.size());
        DeviceSetting deviceSetting = deviceSettingList.get(0);
        assertTrue(deviceSetting.getId() > 0);
        assertEquals(original.getDeviceSettingName(), deviceSetting.getDeviceSettingName());
        assertEquals(original.getDeviceType(), deviceSetting.getDeviceType());
        assertNull(deviceSetting.getDeviceSettingData());
    }

    @Test
    public void test_loadAllDeviceSetting_00003() {
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");
        mDeviceSettingDataSource.insertDeviceSetting(original).blockingSubscribe();

        List<DeviceSetting> deviceSettingList = mDeviceSettingDataSource.loadAllDeviceSetting().blockingGet();
        assertEquals(1, deviceSettingList.size());
        DeviceSetting deviceSetting = deviceSettingList.get(0);
        assertEquals(original.getId(), deviceSetting.getId());
        assertEquals(original.getDeviceSettingName(), deviceSetting.getDeviceSettingName());
        assertEquals(original.getDeviceType(), deviceSetting.getDeviceType());
        assertNull(original.getDeviceSettingData(), deviceSetting.getDeviceSettingData());
    }

    @Test
    public void test_loadDeviceSettingById_00001() {
        final AtomicBoolean result = new AtomicBoolean(false);
        mDeviceSettingDataSource.loadDeviceSettingById(1).blockingSubscribe(deviceSetting -> {
        }, throwable -> result.set(true));
        assertTrue(result.get());
    }

    @Test
    public void test_loadDeviceSettingById_00002() {
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");
        mDeviceSettingDataSource.insertDeviceSetting(original).blockingSubscribe();

        DeviceSetting deviceSetting = mDeviceSettingDataSource.loadDeviceSettingById(original.getId()).blockingGet();
        assertEquals(original.getId(), deviceSetting.getId());
        assertEquals(original.getDeviceSettingName(), deviceSetting.getDeviceSettingName());
        assertEquals(original.getDeviceType(), deviceSetting.getDeviceType());
        assertEquals(original.getDeviceSettingData(), deviceSetting.getDeviceSettingData());
    }

    @Test
    public void test_loadDeviceSettingById_00003() {
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");
        mDeviceSettingDataSource.insertDeviceSetting(original).blockingSubscribe();

        final AtomicBoolean result = new AtomicBoolean(false);
        mDeviceSettingDataSource.loadDeviceSettingById(original.getId() + 1).blockingSubscribe(deviceSetting -> {
        }, throwable -> result.set(true));
        assertTrue(result.get());
    }

    @Test
    public void test_insertDeviceSetting_00001() {
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");
        mDeviceSettingDataSource.insertDeviceSetting(original).blockingSubscribe();

        DeviceSetting deviceSetting = mDeviceSettingDataSource.loadDeviceSettingById(original.getId()).blockingGet();
        assertEquals(original.getId(), deviceSetting.getId());
        assertEquals(original.getDeviceSettingName(), deviceSetting.getDeviceSettingName());
        assertEquals(original.getDeviceType(), deviceSetting.getDeviceType());
        assertEquals(original.getDeviceSettingData(), deviceSetting.getDeviceSettingData());
    }

    @Test
    public void test_insertDeviceSetting_00002() {
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");
        mDeviceSettingDataSource.insertDeviceSetting(original).blockingSubscribe();
        DeviceSetting replaced = new DeviceSetting(1, "aa", 22, "bb");
        mDeviceSettingDataSource.insertDeviceSetting(replaced).blockingSubscribe();

        DeviceSetting deviceSetting = mDeviceSettingDataSource.loadDeviceSettingById(original.getId()).blockingGet();
        assertEquals(replaced.getId(), deviceSetting.getId());
        assertEquals(replaced.getDeviceSettingName(), deviceSetting.getDeviceSettingName());
        assertEquals(replaced.getDeviceType(), deviceSetting.getDeviceType());
        assertEquals(replaced.getDeviceSettingData(), deviceSetting.getDeviceSettingData());
    }

    @Test
    public void test_deleteDeviceSetting_00001() {
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");
        mDeviceSettingDataSource.insertDeviceSetting(original).blockingSubscribe();

        mDeviceSettingDataSource.deleteDeviceSetting(original).blockingSubscribe();
        List<DeviceSetting> deviceSettingList = mDeviceSettingDataSource.loadAllDeviceSetting().blockingGet();
        assertTrue(deviceSettingList.isEmpty());
    }

    @Test
    public void test_deleteDeviceSetting_00002() {
        DeviceSetting original1 = new DeviceSetting(1, "a", 2, "b");
        DeviceSetting original2 = new DeviceSetting(2, "aa", 22, "bb");
        mDeviceSettingDataSource.insertDeviceSetting(original1).blockingSubscribe();
        mDeviceSettingDataSource.insertDeviceSetting(original2).blockingSubscribe();

        mDeviceSettingDataSource.deleteDeviceSetting(original1).blockingSubscribe();

        DeviceSetting deviceSetting = mDeviceSettingDataSource.loadDeviceSettingById(original2.getId()).blockingGet();
        assertEquals(original2.getId(), deviceSetting.getId());
        assertEquals(original2.getDeviceSettingName(), deviceSetting.getDeviceSettingName());
        assertEquals(original2.getDeviceType(), deviceSetting.getDeviceType());
        assertEquals(original2.getDeviceSettingData(), deviceSetting.getDeviceSettingData());
    }

    @Test
    public void test_deleteAllDeviceSetting_00001() {
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");
        mDeviceSettingDataSource.insertDeviceSetting(original).blockingSubscribe();

        mDeviceSettingDataSource.deleteAllDeviceSetting().blockingSubscribe();
        List<DeviceSetting> deviceSettingList = mDeviceSettingDataSource.loadAllDeviceSetting().blockingGet();
        assertTrue(deviceSettingList.isEmpty());
    }

    @Test
    public void test_deleteAllDeviceSetting_00002() {
        DeviceSetting original1 = new DeviceSetting(1, "a", 2, "b");
        DeviceSetting original2 = new DeviceSetting(2, "aa", 22, "bb");
        mDeviceSettingDataSource.insertDeviceSetting(original1).blockingSubscribe();
        mDeviceSettingDataSource.insertDeviceSetting(original2).blockingSubscribe();

        mDeviceSettingDataSource.deleteAllDeviceSetting().blockingSubscribe();
        List<DeviceSetting> deviceSettingList = mDeviceSettingDataSource.loadAllDeviceSetting().blockingGet();
        assertTrue(deviceSettingList.isEmpty());
    }

}