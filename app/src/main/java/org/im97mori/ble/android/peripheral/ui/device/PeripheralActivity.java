package org.im97mori.ble.android.peripheral.ui.device;

import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_ID;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.VALUE_DEVICE_ID_UNSAVED;

import android.Manifest;
import android.bluetooth.BluetoothAdapter;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.graphics.drawable.Animatable;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.content.res.AppCompatResources;
import androidx.core.util.Pair;
import androidx.core.view.MenuProvider;
import androidx.lifecycle.HasDefaultViewModelProviderFactory;
import androidx.lifecycle.ViewModelProvider;
import androidx.room.rxjava3.EmptyResultSetException;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.PeripheralActivityBinding;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingLauncherContract;
import org.im97mori.ble.android.peripheral.utils.AutoDisposeViewModelProvider;
import org.im97mori.stacklog.LogUtils;

import java.util.Optional;
import java.util.function.Function;

import dagger.hilt.android.AndroidEntryPoint;

import javax.inject.Inject;

@AndroidEntryPoint
public class PeripheralActivity extends AppCompatActivity {

    @Inject
    Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> viewModelProviderFactoryFunction;

    private PeripheralViewModel mViewModel;

    private PeripheralActivityBinding mBinding;

    private Integer mDeviceType;

    private Animatable mAnimatedDrawable;

    private final ActivityResultLauncher<Pair<Long, Integer>> mStartDeviceSettingActivity = registerForActivityResult(new DeviceSettingLauncherContract(), result -> {
        if (result) {
            mViewModel.clear();
            fetch();
        }
    });

    private final ActivityResultLauncher<Intent> mEnableBluetoothSettingActivity = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), result -> {
    });

    private final ActivityResultLauncher<String[]> requestPermissionLauncher =
            registerForActivityResult(new ActivityResultContracts.RequestMultiplePermissions(), grantState -> {
            });

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new AutoDisposeViewModelProvider(this, viewModelProviderFactoryFunction.apply(this)).get(PeripheralViewModel.class);

        mBinding = PeripheralActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        Drawable drawable = AppCompatResources.getDrawable(this, R.drawable.peripheral_advertising_background_animation);
        mBinding.deviceTypeImage.setBackground(drawable);
        if (drawable instanceof Animatable) {
            mAnimatedDrawable = (Animatable) drawable;
        }

        mViewModel.observeTitle(this, mBinding.topAppBar::setTitle);
        mViewModel.observeDeviceTypeImageResId(this, mBinding.deviceTypeImage::setImageResource);
        mViewModel.observeDeviceType(this, integer -> mDeviceType = integer);
        mViewModel.observeDeviceTypeName(this, mBinding.deviceTypeName::setText);
        mViewModel.observeIsReady(this, isReady -> mBinding.topAppBar.invalidateMenu());
        mViewModel.observeIsStarted(this, isStarted -> {
            if (mAnimatedDrawable != null) {
                if (isStarted) {
                    mAnimatedDrawable.start();
                } else {
                    mAnimatedDrawable.stop();
                }
            }
            mBinding.topAppBar.invalidateMenu();
        });
        mViewModel.observeIsBluetoothEnabled(this, isEnabled -> {
            if (!isEnabled) {
                mViewModel.quit();
            }
            mBinding.topAppBar.invalidateMenu();
        });

        mBinding.topAppBar.addMenuProvider(new MenuProvider() {
            @Override
            public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                if (mBinding.rootContainer.getVisibility() == View.VISIBLE) {
                    menu.setGroupVisible(R.id.all, true);
                    if (mViewModel.isBluetoothEnabled()) {
                        if (mViewModel.isPeripheralReady()) {
                            if (mViewModel.isPeripheralStarted()) {
                                menu.findItem(R.id.peripheralStart).setEnabled(false);
                                menu.findItem(R.id.peripheralStop).setEnabled(true);
                                menu.findItem(R.id.setting).setEnabled(false);
                                menu.findItem(R.id.delete).setEnabled(false);
                            } else {
                                menu.findItem(R.id.peripheralStart).setEnabled(true);
                                menu.findItem(R.id.peripheralStop).setEnabled(false);
                                menu.findItem(R.id.setting).setEnabled(true);
                                menu.findItem(R.id.delete).setEnabled(true);
                            }
                        } else {
                            menu.findItem(R.id.peripheralStart).setEnabled(false);
                            menu.findItem(R.id.peripheralStop).setEnabled(false);
                            menu.findItem(R.id.setting).setEnabled(false);
                            menu.findItem(R.id.delete).setEnabled(false);
                        }
                        menu.findItem(R.id.bluetooth_enable).setEnabled(false);
                        Optional.ofNullable(menu.findItem(R.id.bluetooth_disable)).ifPresent(menuItem -> menuItem.setEnabled(true));
                    } else {
                        menu.findItem(R.id.peripheralStart).setEnabled(false);
                        menu.findItem(R.id.peripheralStop).setEnabled(false);
                        menu.findItem(R.id.setting).setEnabled(true);
                        menu.findItem(R.id.delete).setEnabled(true);

                        menu.findItem(R.id.bluetooth_enable).setEnabled(true);
                        Optional.ofNullable(menu.findItem(R.id.bluetooth_disable)).ifPresent(menuItem -> menuItem.setEnabled(false));
                    }
                } else {
                    menu.setGroupVisible(R.id.all, false);
                }
            }

            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                boolean result;
                if (R.id.peripheralStart == menuItem.getItemId()) {
                    if (checkPermission()) {
                        mViewModel.start();
                    }
                    result = true;
                } else if (R.id.peripheralStop == menuItem.getItemId()) {
                    if (checkPermission()) {
                        mViewModel.quit();
                    }
                    result = true;
                } else if (R.id.setting == menuItem.getItemId()) {
                    mStartDeviceSettingActivity.launch(Pair.create(getIntent().getLongExtra(KEY_DEVICE_ID, VALUE_DEVICE_ID_UNSAVED)
                            , mDeviceType));
                    result = true;
                } else if (R.id.delete == menuItem.getItemId()) {
                    mViewModel.observeDeleteDeviceSetting(getIntent(), () -> finish());
                    result = true;
                } else if (R.id.bluetooth_enable == menuItem.getItemId()) {
                    if (Build.VERSION_CODES.TIRAMISU <= Build.VERSION.SDK_INT) {
                        mEnableBluetoothSettingActivity.launch(new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE));
                    } else {
                        if (checkPermission()) {
                            mViewModel.bluetoothEnable();
                        }
                    }
                    result = true;
                } else if (R.id.bluetooth_disable == menuItem.getItemId()) {
                    if (checkPermission()) {
                        mViewModel.bluetoothDisable();
                    }
                    result = true;
                } else {
                    result = false;
                }
                return result;
            }
        });
    }

    @Override
    protected void onStart() {
        super.onStart();
        fetch();
    }

    @Override
    protected void onDestroy() {
        mViewModel.quit();
        super.onDestroy();
    }

    private boolean checkPermission() {
        boolean result;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            if (PackageManager.PERMISSION_DENIED == checkSelfPermission(Manifest.permission.BLUETOOTH_CONNECT)
                    || PackageManager.PERMISSION_DENIED == checkSelfPermission(Manifest.permission.BLUETOOTH_ADVERTISE)) {
                result = false;
                if (shouldShowRequestPermissionRationale(Manifest.permission.BLUETOOTH_CONNECT)
                        || shouldShowRequestPermissionRationale(Manifest.permission.BLUETOOTH_ADVERTISE)) {
                    Toast.makeText(this, "Need permission", Toast.LENGTH_SHORT).show();
                } else {
                    requestPermissionLauncher.launch(new String[]{Manifest.permission.BLUETOOTH_CONNECT, Manifest.permission.BLUETOOTH_ADVERTISE});
                }
            } else {
                result = true;
            }
        } else {
            result = true;
        }
        return result;
    }

    private void fetch() {
        mViewModel.observeSetup(getIntent()
                , () -> {
                    mBinding.rootContainer.setVisibility(View.VISIBLE);
                    mBinding.topAppBar.invalidateMenu();
                }, throwable -> {
                    LogUtils.stackLog(throwable.getMessage());
                    if (throwable instanceof EmptyResultSetException) {
                        finish();
                    }
                });
    }

}