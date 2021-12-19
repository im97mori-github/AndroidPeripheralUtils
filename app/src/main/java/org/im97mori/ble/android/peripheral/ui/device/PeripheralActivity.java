package org.im97mori.ble.android.peripheral.ui.device;

import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;
import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.core.util.Pair;
import androidx.core.view.MenuProvider;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.appbar.MaterialToolbar;

import org.im97mori.ble.MockData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.Constants;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingLauncherContract;
import org.im97mori.ble.profile.blp.peripheral.BloodPressureProfileMockCallback;
import org.im97mori.ble.service.bls.peripheral.BloodPressureServiceMockCallback;
import org.im97mori.ble.service.dis.peripheral.DeviceInformationServiceMockCallback;

import java.util.Collections;
import java.util.Objects;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class PeripheralActivity extends BaseActivity {

    private PeripheralViewModel mViewModel;

    private BloodPressureProfileMockCallback mCallback;

    private MaterialToolbar mToolBar;

    private final ActivityResultLauncher<Pair<Long, Integer>> mStartDeviceSettingActivity = registerForActivityResult(new DeviceSettingLauncherContract(), result -> {
        if (result) {
            mCallback = null;
            mToolBar.invalidateMenu();
            fetch();
        }
    });

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new ViewModelProvider(this).get(PeripheralViewModel.class);

        setContentView(R.layout.peripheral_activity);

        mToolBar = findViewById(R.id.topAppBar);
        mToolBar.addMenuProvider(new MenuProvider() {
            @Override
            public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                getMenuInflater().inflate(R.menu.peripheral, menu);
                if (mCallback == null) {
                    menu.findItem(R.id.peripheralStart).setEnabled(false);
                    menu.findItem(R.id.peripheralStop).setEnabled(false);
                    menu.findItem(R.id.setting).setEnabled(false);
                } else {
                    if (mCallback.isStarted()) {
                        menu.findItem(R.id.peripheralStart).setEnabled(false);
                        menu.findItem(R.id.peripheralStop).setEnabled(true);
                        menu.findItem(R.id.setting).setEnabled(false);
                    } else {
                        menu.findItem(R.id.peripheralStart).setEnabled(true);
                        menu.findItem(R.id.peripheralStop).setEnabled(false);
                        menu.findItem(R.id.setting).setEnabled(true);
                    }
                }
            }

            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                return PeripheralActivity.this.onOptionsItemSelected(menuItem);
            }
        });
    }

    @Override
    protected void onStart() {
        super.onStart();
        if (mCallback == null) {
            fetch();
        }
    }

    @Override
    protected void onDestroy() {
        if (mCallback != null) {
            mCallback.quit();
        }
        super.onDestroy();
    }

    private void fetch() {
        mDisposable.add(mViewModel.setup(getIntent())
                .subscribe((mockData, throwable) -> {
                    MockData blsMockData = null;
                    MockData disMockData = null;
                    for (ServiceData serviceData : mockData.serviceDataList) {
                        if (BLOOD_PRESSURE_SERVICE.equals(serviceData.uuid)) {
                            blsMockData = new MockData(Collections.singletonList(serviceData));
                        } else if (DEVICE_INFORMATION_SERVICE.equals(serviceData.uuid)) {
                            disMockData = new MockData(Collections.singletonList(serviceData));
                        }
                    }
                    mCallback = new BloodPressureProfileMockCallback(this
                            , new BloodPressureServiceMockCallback(Objects.requireNonNull(blsMockData), false)
                            , disMockData == null ? null : new DeviceInformationServiceMockCallback(disMockData, false));
                    mToolBar.invalidateMenu();
                }));
    }

    @Override
    public boolean onOptionsItemSelected(@NonNull MenuItem item) {
        boolean result;
        if (R.id.peripheralStart == item.getItemId()) {
            mCallback.start();
            result = true;
        } else if (R.id.peripheralStop == item.getItemId()) {
            mCallback.quit();
            result = true;
        } else if (R.id.setting == item.getItemId()) {
            mStartDeviceSettingActivity.launch(Pair.create(getIntent().getLongExtra(DeviceSettingLauncherContract.KEY_DEVICE_ID, DeviceSettingLauncherContract.UNSAVED_DEVICE_ID), Constants.DeviceTypes.DEVICE_TYPE_UNDEFINED));
            result = true;
        } else {
            result = super.onOptionsItemSelected(item);
        }
        return result;
    }

}