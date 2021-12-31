package org.im97mori.ble.android.peripheral.ui.device;

import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_ID;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.VALUE_DEVICE_ID_UNSAVED;

import android.graphics.drawable.AnimatedVectorDrawable;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.util.Pair;
import androidx.core.view.MenuProvider;
import androidx.room.rxjava3.EmptyResultSetException;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.PeripheralActivityBinding;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingLauncherContract;
import org.im97mori.ble.android.peripheral.utils.MockitoViewModelProvider;
import org.im97mori.stacklog.LogUtils;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class PeripheralActivity extends AppCompatActivity {

    private PeripheralViewModel mViewModel;

    private PeripheralActivityBinding mBinding;

    private Integer mDeviceType;

    private final ActivityResultLauncher<Pair<Long, Integer>> mStartDeviceSettingActivity = registerForActivityResult(new DeviceSettingLauncherContract(), result -> {
        if (result) {
            mViewModel.clear();
            fetch();
        }
    });

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewModel = new MockitoViewModelProvider(this).get(PeripheralViewModel.class);

        mBinding = PeripheralActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mViewModel.observeTitle(this, mBinding.topAppBar::setTitle);
        mViewModel.observeTypeImageRes(this, mBinding.deviceTypeImage::setImageResource);
        mViewModel.observeDeviceType(this, integer -> mDeviceType = integer);
        mViewModel.observeDeviceTypeName(this, mBinding.deviceTypeName::setText);
        mViewModel.observeIsReady(this, isReady -> mBinding.topAppBar.invalidateMenu());
        mViewModel.observeIsStarted(this, isStarted -> {
            AnimatedVectorDrawable animatedVectorDrawable = (AnimatedVectorDrawable) mBinding.deviceTypeImage.getBackground();
            if (isStarted) {
                animatedVectorDrawable.start();
            } else {
                animatedVectorDrawable.stop();
            }
            mBinding.topAppBar.invalidateMenu();
        });

        mBinding.topAppBar.addMenuProvider(new MenuProvider() {
            @Override
            public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
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
            }

            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                boolean result;
                if (R.id.peripheralStart == menuItem.getItemId()) {
                    mViewModel.start();
                    result = true;
                } else if (R.id.peripheralStop == menuItem.getItemId()) {
                    mViewModel.quit();
                    result = true;
                } else if (R.id.setting == menuItem.getItemId()) {
                    mStartDeviceSettingActivity.launch(Pair.create(getIntent().getLongExtra(KEY_DEVICE_ID, VALUE_DEVICE_ID_UNSAVED)
                            , mDeviceType));
                    result = true;
                } else if (R.id.delete == menuItem.getItemId()) {
                    mViewModel.observeDeleteDeviceSetting(getIntent(), () -> finish());
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

    private void fetch() {
        if (!mViewModel.isPeripheralReady()) {
            mViewModel.observeSetup(getIntent(), () -> mBinding.rootContainer.setVisibility(View.VISIBLE), throwable -> {
                LogUtils.stackLog(throwable.getMessage());
                if (throwable instanceof EmptyResultSetException) {
                    finish();
                }
            });
        }
    }

}