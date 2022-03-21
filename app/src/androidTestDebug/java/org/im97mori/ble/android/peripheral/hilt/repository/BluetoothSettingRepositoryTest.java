package org.im97mori.ble.android.peripheral.hilt.repository;

import static android.bluetooth.BluetoothAdapter.STATE_OFF;
import static android.bluetooth.BluetoothAdapter.STATE_ON;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_UNDEFINED;
import static org.im97mori.ble.android.peripheral.utils.Utils.stackLog;
import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothGattService;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;

import org.im97mori.ble.BLEUtilsAndroid;
import org.im97mori.ble.MockData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.im97mori.ble.android.peripheral.utils.Utils;
import org.im97mori.ble.profile.blp.peripheral.BloodPressureProfileMockCallback;
import org.im97mori.ble.profile.peripheral.AbstractProfileMockCallback;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import java.util.Collections;
import java.util.LinkedList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;

@HiltAndroidTest
public class BluetoothSettingRepositoryTest {

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Inject
    BluetoothSettingRepository mBluetoothSettingRepository;

    @Inject
    @ApplicationContext
    Context mContext;

    @Before
    public void setUp() {
        mHiltRule.inject();
    }

    @Test
    public void test_addBluetoothStatusConsumer_00001() {
        AtomicReference<Boolean> result = new AtomicReference<>();
        Consumer<Boolean> consumer = result::set;

        CountDownLatch countDownLatch = new CountDownLatch(BLEUtilsAndroid.isBluetoothEnabled(mContext) ? 2 : 1);
        BroadcastReceiver broadcastReceiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                if (countDownLatch.getCount() == 2) {
                    if (intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, STATE_OFF) == STATE_OFF) {
                        mBluetoothSettingRepository.addBluetoothStatusConsumer(consumer);
                        mBluetoothSettingRepository.bluetoothEnable();
                        countDownLatch.countDown();
                    }
                }
                if (countDownLatch.getCount() == 1) {
                    if (intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, STATE_OFF) == STATE_ON) {
                        countDownLatch.countDown();
                    }
                }
            }
        };
        mContext.registerReceiver(broadcastReceiver, new IntentFilter(BluetoothAdapter.ACTION_STATE_CHANGED));

        if (countDownLatch.getCount() == 2) {
            mBluetoothSettingRepository.bluetoothDisable();
        } else {
            mBluetoothSettingRepository.addBluetoothStatusConsumer(consumer);
            mBluetoothSettingRepository.bluetoothEnable();
        }

        boolean done = false;
        do {
            try {
                done = countDownLatch.await(1, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                stackLog(e);
            }
        } while (!done);

        mContext.unregisterReceiver(broadcastReceiver);
        assertTrue(result.get());
        mBluetoothSettingRepository.removeBluetoothStatusConsumer(consumer);
    }

    @Test
    public void test_addBluetoothStatusConsumer_00002() {
        AtomicInteger count = new AtomicInteger(0);
        Consumer<Boolean> consumer = aBoolean -> {
            if (aBoolean) {
                count.incrementAndGet();
            }
        };

        CountDownLatch countDownLatch = new CountDownLatch(BLEUtilsAndroid.isBluetoothEnabled(mContext) ? 2 : 1);
        BroadcastReceiver broadcastReceiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                if (countDownLatch.getCount() == 2) {
                    if (intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, STATE_OFF) == STATE_OFF) {
                        mBluetoothSettingRepository.addBluetoothStatusConsumer(consumer);
                        mBluetoothSettingRepository.addBluetoothStatusConsumer(consumer);
                        mBluetoothSettingRepository.bluetoothEnable();
                        countDownLatch.countDown();
                    }
                }
                if (countDownLatch.getCount() == 1) {
                    if (intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, STATE_OFF) == STATE_ON) {
                        countDownLatch.countDown();
                    }
                }
            }
        };
        mContext.registerReceiver(broadcastReceiver, new IntentFilter(BluetoothAdapter.ACTION_STATE_CHANGED));

        if (countDownLatch.getCount() == 2) {
            mBluetoothSettingRepository.bluetoothDisable();
        } else {
            mBluetoothSettingRepository.addBluetoothStatusConsumer(consumer);
            mBluetoothSettingRepository.addBluetoothStatusConsumer(consumer);
            mBluetoothSettingRepository.bluetoothEnable();
        }

        boolean done = false;
        do {
            try {
                done = countDownLatch.await(1, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                stackLog(e);
            }
        } while (!done);

        mContext.unregisterReceiver(broadcastReceiver);
        assertEquals(1, count.get());
    }

    @Test
    public void test_removeBluetoothStatusConsume_00001() {
        AtomicReference<Boolean> result = new AtomicReference<>();
        Consumer<Boolean> consumer = result::set;

        CountDownLatch countDownLatch = new CountDownLatch(BLEUtilsAndroid.isBluetoothEnabled(mContext) ? 2 : 1);
        BroadcastReceiver broadcastReceiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                if (countDownLatch.getCount() == 2) {
                    if (intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, STATE_OFF) == STATE_OFF) {
                        mBluetoothSettingRepository.addBluetoothStatusConsumer(consumer);
                        mBluetoothSettingRepository.removeBluetoothStatusConsumer(consumer);
                        mBluetoothSettingRepository.bluetoothEnable();
                        countDownLatch.countDown();
                    }
                }
                if (countDownLatch.getCount() == 1) {
                    if (intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, STATE_OFF) == STATE_ON) {
                        countDownLatch.countDown();
                    }
                }
            }
        };
        mContext.registerReceiver(broadcastReceiver, new IntentFilter(BluetoothAdapter.ACTION_STATE_CHANGED));

        if (countDownLatch.getCount() == 2) {
            mBluetoothSettingRepository.bluetoothDisable();
        } else {
            mBluetoothSettingRepository.addBluetoothStatusConsumer(consumer);
            mBluetoothSettingRepository.removeBluetoothStatusConsumer(consumer);
            mBluetoothSettingRepository.bluetoothEnable();
        }

        try {
            //noinspection ResultOfMethodCallIgnored
            countDownLatch.await(1, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            stackLog(e);
        }

        mContext.unregisterReceiver(broadcastReceiver);
        assertNull(result.get());
    }

    @Test
    public void test_isBluetoothEnabled_00001() {
        CountDownLatch countDownLatch = new CountDownLatch(BLEUtilsAndroid.isBluetoothEnabled(mContext) ? 2 : 1);
        BroadcastReceiver broadcastReceiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                if (countDownLatch.getCount() == 2) {
                    if (intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, STATE_OFF) == STATE_OFF) {
                        mBluetoothSettingRepository.bluetoothEnable();
                        countDownLatch.countDown();
                    }
                }
                if (countDownLatch.getCount() == 1) {
                    if (intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, STATE_OFF) == STATE_ON) {
                        countDownLatch.countDown();
                    }
                }
            }
        };
        mContext.registerReceiver(broadcastReceiver, new IntentFilter(BluetoothAdapter.ACTION_STATE_CHANGED));

        if (countDownLatch.getCount() == 2) {
            mBluetoothSettingRepository.bluetoothDisable();
        } else {
            mBluetoothSettingRepository.bluetoothEnable();
        }

        boolean done = false;
        do {
            try {
                done = countDownLatch.await(1, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                stackLog(e);
            }
        } while (!done);

        mContext.unregisterReceiver(broadcastReceiver);
        assertTrue(mBluetoothSettingRepository.isBluetoothEnabled());
    }

    @Test
    public void test_isBluetoothEnabled_00002() {
        CountDownLatch countDownLatch = new CountDownLatch(BLEUtilsAndroid.isBluetoothEnabled(mContext) ? 1 : 2);
        BroadcastReceiver broadcastReceiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                if (countDownLatch.getCount() == 2) {
                    if (intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, STATE_OFF) == STATE_ON) {
                        mBluetoothSettingRepository.bluetoothDisable();
                        countDownLatch.countDown();
                    }
                }
                if (countDownLatch.getCount() == 1) {
                    if (intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, STATE_OFF) == STATE_OFF) {
                        countDownLatch.countDown();
                    }
                }
            }
        };
        mContext.registerReceiver(broadcastReceiver, new IntentFilter(BluetoothAdapter.ACTION_STATE_CHANGED));

        if (countDownLatch.getCount() == 2) {
            mBluetoothSettingRepository.bluetoothEnable();
        } else {
            mBluetoothSettingRepository.bluetoothDisable();
        }

        boolean done = false;
        do {
            try {
                done = countDownLatch.await(1, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                stackLog(e);
            }
        } while (!done);

        mContext.unregisterReceiver(broadcastReceiver);
        assertFalse(mBluetoothSettingRepository.isBluetoothEnabled());
    }

    @Test
    public void test_bluetoothEnable_00001() {
        CountDownLatch countDownLatch = new CountDownLatch(BLEUtilsAndroid.isBluetoothEnabled(mContext) ? 2 : 1);
        BroadcastReceiver broadcastReceiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                if (countDownLatch.getCount() == 2) {
                    if (intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, STATE_OFF) == STATE_OFF) {
                        mBluetoothSettingRepository.bluetoothEnable();
                        countDownLatch.countDown();
                    }
                }
                if (countDownLatch.getCount() == 1) {
                    if (intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, STATE_OFF) == STATE_ON) {
                        countDownLatch.countDown();
                    }
                }
            }
        };
        mContext.registerReceiver(broadcastReceiver, new IntentFilter(BluetoothAdapter.ACTION_STATE_CHANGED));

        if (countDownLatch.getCount() == 2) {
            mBluetoothSettingRepository.bluetoothDisable();
        } else {
            mBluetoothSettingRepository.bluetoothEnable();
        }

        boolean done = false;
        do {
            try {
                done = countDownLatch.await(1, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                stackLog(e);
            }
        } while (!done);

        mContext.unregisterReceiver(broadcastReceiver);
        assertTrue(mBluetoothSettingRepository.isBluetoothEnabled());
    }

    @Test
    public void test_bluetoothDisable_00002() {
        CountDownLatch countDownLatch = new CountDownLatch(BLEUtilsAndroid.isBluetoothEnabled(mContext) ? 1 : 2);
        BroadcastReceiver broadcastReceiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                if (countDownLatch.getCount() == 2) {
                    if (intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, STATE_OFF) == STATE_ON) {
                        mBluetoothSettingRepository.bluetoothDisable();
                        countDownLatch.countDown();
                    }
                }
                if (countDownLatch.getCount() == 1) {
                    if (intent.getIntExtra(BluetoothAdapter.EXTRA_STATE, STATE_OFF) == STATE_OFF) {
                        countDownLatch.countDown();
                    }
                }
            }
        };
        mContext.registerReceiver(broadcastReceiver, new IntentFilter(BluetoothAdapter.ACTION_STATE_CHANGED));

        if (countDownLatch.getCount() == 2) {
            mBluetoothSettingRepository.bluetoothEnable();
        } else {
            mBluetoothSettingRepository.bluetoothDisable();
        }

        boolean done = false;
        do {
            try {
                done = countDownLatch.await(1, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                stackLog(e);
            }
        } while (!done);

        mContext.unregisterReceiver(broadcastReceiver);
        assertFalse(mBluetoothSettingRepository.isBluetoothEnabled());
    }

    @Test
    public void test_createProfileMockCallback_00001() {
        MockData mockData = new MockData(new LinkedList<>());
        Exception exception = null;
        try {
            mBluetoothSettingRepository.createProfileMockCallback(new DeviceSetting(1, "", DEVICE_TYPE_UNDEFINED, Utils.parcelableToByteArray(mockData)));
        } catch (Exception e) {
            exception = e;
        }
        assertNotNull(exception);
        assertEquals("Not Found", exception.getMessage());
    }

    @Test
    public void test_createProfileMockCallback_00002() {
        MockData mockData = new MockData(new LinkedList<>());
        mockData.serviceDataList.add(new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList()));
        AbstractProfileMockCallback callback = mBluetoothSettingRepository.createProfileMockCallback(new DeviceSetting(1, "", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, Utils.parcelableToByteArray(mockData)));
        assertTrue(callback instanceof BloodPressureProfileMockCallback);
    }

}