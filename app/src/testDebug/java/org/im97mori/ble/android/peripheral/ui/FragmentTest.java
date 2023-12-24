package org.im97mori.ble.android.peripheral.ui;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mockStatic;

import android.content.Context;
import android.os.Build;

import androidx.fragment.app.Fragment;
import androidx.lifecycle.Lifecycle;
import androidx.test.espresso.intent.Intents;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.test.HiltTestActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.FakeDeviceSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp.BloodPressureProfileViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp.FakeBloodPressureProfileViewModel;
import org.im97mori.ble.android.peripheral.utils.AutoDisposeViewModelProvider;
import org.im97mori.test.android.NestedFragmentScenario2;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import io.reactivex.rxjava3.android.plugins.RxAndroidPlugins;
import io.reactivex.rxjava3.plugins.RxJavaPlugins;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class FragmentTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Inject
    @ApplicationContext
    Context mContext;

    private static MockedStatic<AutoDisposeViewModelProvider> mockedStatic;

    private FakeBloodPressureProfileViewModel mFakeBloodPressureProfileViewModel;

    @BeforeClass
    public static void setUpClass() {
        mockedStatic = mockStatic(AutoDisposeViewModelProvider.class);
        mockedStatic.when(() -> AutoDisposeViewModelProvider.getViewModelClass(DeviceSettingViewModel.class))
                .thenReturn(FakeDeviceSettingViewModel.class);
        mockedStatic.when(() -> AutoDisposeViewModelProvider.getViewModelClass(BloodPressureProfileViewModel.class))
                .thenReturn(FakeBloodPressureProfileViewModel.class);
    }

    @AfterClass
    public static void tearDownClass() {
        mockedStatic.close();
    }

    @Before
    public void setUp() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mHiltRule.inject();
        Intents.init();
    }

    @After
    public void tearDown() {
        Intents.release();
    }

    @Test
    public void test_launchInContainer_001() {
        try (NestedFragmentScenario2<ChildFragment, HiltTestActivity, ParentFragment> scenario
                     = NestedFragmentScenario2.launchInContainer(ChildFragment.class, HiltTestActivity.class, ParentFragment.class)) {
            scenario.onFragment(childFragment -> assertTrue(childFragment.requireParentFragment() instanceof ParentFragment));
        }
    }

    @Test
    public void test_launchInContainer_002() {
        try (NestedFragmentScenario2<ChildFragment, HiltTestActivity, ParentFragment> scenario
                     = NestedFragmentScenario2.launchInContainer(ChildFragment.class
                , null
                , HiltTestActivity.class
                , ParentFragment.class
                , R.style.Theme_AndroidPeripheralUtils
                , Lifecycle.State.RESUMED
                , null
                , R.id.fragmentParentContainer)) {
            scenario.onFragment(childFragment -> {
                Fragment parentFragment = childFragment.requireParentFragment();
                assertTrue(parentFragment instanceof ParentFragment);
                assertEquals(childFragment, parentFragment.getChildFragmentManager().findFragmentById(R.id.fragmentParentContainer));
            });
        }
    }

    @Test
    public void test_launch_001() {
        try (NestedFragmentScenario2<ChildFragment, HiltTestActivity, ParentFragment> scenario
                     = NestedFragmentScenario2.launch(ChildFragment.class, HiltTestActivity.class, ParentFragment.class)) {
            scenario.onFragment(childFragment -> assertTrue(childFragment.requireParentFragment() instanceof ParentFragment));
        }
    }

    @Test
    public void test_launch_002() {
        try (NestedFragmentScenario2<ChildDialogFragment, HiltTestActivity, ParentFragment> scenario
                     = NestedFragmentScenario2.launch(ChildDialogFragment.class
                , null
                , HiltTestActivity.class
                , ParentFragment.class
                , R.style.Theme_AndroidPeripheralUtils)) {
            scenario.onFragment(childFragment -> assertTrue(childFragment.requireParentFragment() instanceof ParentFragment));
        }
    }
}